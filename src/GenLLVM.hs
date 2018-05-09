{- |
Module      : GenLLVM
Description : Generate the llvm code for the program
Copyright   : Modified by Jason Mittertreiner, 2018
-}

{-
Copyright (c) 2013-2016, Stephen Diehl

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to
deal in the Software without restriction, including without limitation the
rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
sell copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
IN THE SOFTWARE.
-}
{-# LANGUAGE OverloadedStrings #-}
module GenLLVM where

import LLVM.Module
import LLVM.Context
import LLVM.Prelude
import LLVM.Prelude

import qualified LLVM.AST as AST
import qualified LLVM.AST.Type
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.FloatingPointPredicate as FP
import qualified LLVM.AST.IntegerPredicate as IP
import qualified Data.Map as M

import Control.Monad.Except

import Lib.Llvm
import Lib.Types as TP
import qualified Ast.TypedAst as TA

-- | Generate LLVM for a program
genProg :: TA.Prog -> LLVM ()
genProg (TA.Prog funcs) = forM_ funcs genFunc

-- | Generate LLVM for a function
genFunc :: TA.Function -> LLVM ()
genFunc (TA.Func tpe qname args bdy fret) = do
  function (toLLVMType tpe) (AST.mkName (show qname)) llvmargs blks
  where
    llvmargs = [(toLLVMType t, AST.mkName (show n)) | (t, n) <-args ]
    blks = createBlocks $ execCodegen $ do
      -- Create and enter a new block for the function
      entry <- addBlock entryBlockName
      _ <- setBlock entry
      -- Allocate and declare the function arguments
      forM_ llvmargs $ \(t, (AST.Name n)) -> do
        var <- alloca t
        store var (local t (AST.Name n))
        declVar (AST.Name n) var
      -- Generate the body
      forM_ bdy genStm
      -- Return
      genExpr fret >>= ret

genExpr :: TA.Expr -> Codegen AST.Operand
genExpr (TA.BOp TP.Assign (TA.Var name _ _ _) exp tpe) = do
  var' <- varToOp name
  exp' <- genExpr exp
  store var' exp'
  return exp'
genExpr (TA.BOp TP.Assign exp1 exp2 tpe) = do
  exp1' <- genExpr exp1
  exp2' <- genExpr exp2
  store exp1' exp2'
  return exp1'
genExpr (TA.BOp op e1 e2 _) = do
  let op' = bopToLLVMBop (TA.getExprType e1) op
  e1' <- genExpr e1
  e2' <- genExpr e2
  op' e1' e2'
genExpr (TA.EAssign name exp tpe) = do
  var' <- varToOp name
  exp' <- genExpr exp
  store var' exp'
  return exp'
genExpr (TA.UOp op e1 t) = do
  let op' = uopToLLVMUop (TA.getExprType e1) op
  undefined
genExpr (TA.Var x _ tpe _) = do
  v <- varToOp x
  load v (toLLVMType tpe)
genExpr (TA.FLit i sz) = return . constOp . C.Float $ case sz of
  TP.F32 -> F.Single (realToFrac i)
  TP.F64 -> F.Double i
  TP.FUnspec -> error "Sizes should already be determined"
genExpr (TA.Lit i sz _) = return . constOp $ case sz of
  TP.I1 -> C.Int 1 (fromIntegral i)
  TP.I8 -> C.Int 8 (fromIntegral i)
  TP.I16 -> C.Int 16 (fromIntegral i)
  TP.I32 -> C.Int 32 (fromIntegral i)
  TP.I64 -> C.Int 64 (fromIntegral i)
  TP.IUnspec -> error "Sizes should already be determined"
genExpr (TA.Call fn _ args tpe) = do
  let tpe' = toLLVMType tpe
  largs <- mapM genExpr args
  call (externf tpe' (AST.mkName (show fn))) largs tpe'
genExpr (TA.CCall name args) = undefined

genStm :: TA.Statement -> Codegen AST.Operand
genStm (TA.SBlock s _) = do
  forM_ s genStm
  return $ constOp $ C.Int 1 0
genStm (TA.SExpr e _) = genExpr e
genStm (TA.SDecl name tpe _) = do
  let name' = (AST.mkName (show name))
  var <- alloca (toLLVMType tpe)
  declVar name' var
  return $ constOp $ C.Int 1 0
genStm (TA.SDeclAssign name tpe e _) = do
  let name' = (AST.mkName (show name))
  var <- alloca (toLLVMType tpe)
  e' <- genExpr e
  declVar name' var
  store var e'
  return e'
genStm (TA.SWhile e bdy _) = do
  whiletest <- addBlock "while.test"
  whileloop <- addBlock "while.loop"
  whileexit <- addBlock "while.exit"
  br whiletest
  setBlock whiletest
  cond <- genExpr e
  test <- cmp LLVM.AST.Type.i1 IP.NE (constOp (C.Int 1 0)) cond
  cbr test whileloop whileexit
  setBlock whileloop
  genStm bdy
  br whiletest
  setBlock whileexit
  return $ constOp $ C.Int 1 0
genStm (TA.SIf e bdy _) = do
  ifblock <- addBlock "if.block"
  ifend <- addBlock "if.exit"
  cond <- genExpr e
  test <- cmp LLVM.AST.Type.i1 IP.NE (constOp (C.Int 1 0)) cond
  cbr test ifblock ifend
  setBlock ifblock
  genStm bdy
  setBlock ifblock
  br ifend
  setBlock ifend
  return $ constOp $ C.Int 1 0

genLLVM :: AST.Module -> M.Map ModulePath TA.Prog -> Either String AST.Module
genLLVM mod modules = Right newast
  where
    modn    = forM (M.elems modules) genProg
    newast = runLLVM mod modn
