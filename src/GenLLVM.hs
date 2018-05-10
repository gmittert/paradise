{- |
Module      : GenLLVM
Description : Generate the llvm code for the program
Copyright   : Jason Mittertreiner, 2018
-}

{-# LANGUAGE OverloadedStrings #-}
module GenLLVM where

import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.IntegerPredicate as IP
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Instruction
import LLVM.IRBuilder.Constant
import qualified Data.Map as M

import Control.Monad.Except
import Errors.CompileError

import Lib.Llvm
import Lib.Types as TP
import qualified Ast.TypedAst as TA

-- | Generate LLVM for a program
genProg :: [TA.Prog] -> AST.Module
genProg progs =
  let funcs = join $ (\(TA.Prog func) -> func) <$> progs in
    evalCodegen $ buildModuleT "main" (forM funcs genFunc)

-- | Generate LLVM for a function
genFunc :: TA.Function -> ModuleBuilderT Codegen AST.Operand
genFunc (TA.Func tpe qname args bdy fret) = do
  function (qn2n qname) llvmargs (toLLVMType tpe) $ \largs -> do
      -- Create and enter a new block for the function
      emitBlockStart "entry"
      -- Declare the function arguments
      forM_ (zip (map snd args) largs) (\(name, arg) -> declvar (tn2n name) arg)
      -- Generate the body
      forM_ bdy genStm
      -- Return
      r <- genExpr fret
      ret r
  where
    nameToSbs n = let (AST.Name sbs) = tn2n n in sbs
    llvmargs = [(toLLVMType t, ParameterName (nameToSbs n)) | (t, n) <-args ]

genExpr :: TA.Expr -> LLVMGen AST.Operand
-- | If a name appears as an lval, get the address, not the value
genExpr (TA.BOp TP.Assign (TA.Var name _ _ _) exp _) = do
  var' <- getvar (tn2n name)
  exp' <- genExpr exp
  store var' 0 exp'
  return exp'
genExpr (TA.BOp op e1 e2 _) = do
  let op' = bopToLLVMBop (TA.getExprType e1) (TA.getExprType e2) op
  e1' <- genExpr e1
  e2' <- genExpr e2
  op' e1' e2'

genExpr (TA.UOp op e1 t) = do
  let op' = uopToLLVMUop (TA.getExprType e1) op
  undefined
genExpr (TA.Var x _ tpe _) = do
  v <- getvar (tn2n x)
  load v 0
genExpr (TA.FLit i sz) = case sz of
  TP.F32 -> single (realToFrac i)
  TP.F64 -> double i
  TP.FUnspec -> double i
genExpr (TA.Lit i sz _) = case sz of
  TP.I1 -> bit (fromIntegral i)
  TP.I8 -> byte (fromIntegral i)
  TP.I16 -> word (fromIntegral i)
  TP.I32 -> int32 (fromIntegral i)
  TP.I64 -> int64 (fromIntegral i)
  TP.IUnspec -> int64 (fromIntegral i)
genExpr (TA.ArrLit exprs tpe@(TP.Arr elemTpe _)) = do
  arrvar <- alloca (toLLVMType tpe) Nothing 0
  let assigns = zip exprs [0, (TP.toSize elemTpe)..]
  forM_ assigns (\(e, i)-> do
           e' <- genExpr e
           ptr <- gep arrvar (int64 0 ++ int64 (fromIntegral i))
           store ptr 0 e')
  return arrvar
genExpr (TA.Call fn _ args tpe) = do
  let tpe' = toLLVMType tpe
  largs <- mapM genExpr args
  let params = map (\x -> (x, [])) largs
  func <- getfunc (qn2n fn)
  call func params
genExpr (TA.CCall name args) = undefined

genStm :: TA.Statement -> LLVMGen ()
genStm (TA.SBlock s _) = forM_ s genStm
genStm (TA.SExpr e _) = Control.Monad.Except.void (genExpr e)
genStm (TA.SDecl name tpe _) = do
  let name' = AST.mkName (show name)
  var <- alloca (toLLVMType tpe) Nothing 0
  declvar name' var
genStm (TA.SDeclAssign name tpe e _) = do
  let name' = AST.mkName (show name)
  var <- alloca (toLLVMType tpe) Nothing 0
  e' <- genExpr e
  declvar name' var
  store var 0 e'
genStm (TA.SWhile e bdy _) = do
  whiletest <- freshName "while.test"
  whileloop <- freshName "while.loop"
  whileexit <- freshName "while.exit"
  emitBlockStart whiletest
  cond <- genExpr e
  test <- icmp IP.NE (AST.ConstantOperand (C.Int 1 0)) cond
  condBr test whileloop whileexit
  emitBlockStart whileloop
  genStm bdy
  br whiletest
  emitBlockStart whileexit
genStm (TA.SIf e bdy _) = do
  ifblock <- freshName "if.block"
  ifend <- freshName "if.exit"
  cond <- genExpr e
  test <- icmp IP.NE (AST.ConstantOperand (C.Int 1 0)) cond
  condBr test ifblock ifend
  emitBlockStart ifblock
  genStm bdy
  emitBlockStart ifend

genLLVM :: M.Map ModulePath TA.Prog -> Either CompileError AST.Module
genLLVM modules = Right (genProg (M.elems modules))
