{- |
Module      : GenLLVM
Description : Generate the llvm code for the program
Copyright   : Modified by Jason Mittertreiner, 2017
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
module Lib.Llvm where

import Control.Monad.State

import LLVM.AST
import LLVM.AST.Type
import qualified LLVM.AST.Constant as C
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Instruction
import LLVM.IRBuilder.Constant
import LLVM.IRBuilder.Module

import qualified LLVM.AST.FloatingPointPredicate as FP
import qualified LLVM.AST.IntegerPredicate as IP

import Data.ByteString.Short.Internal
import qualified Lib.Types as T

-------------------------------------------------------------------------------
-- Codegen State
-------------------------------------------------------------------------------

data CodegenState
  = CodegenState {
    locals :: [(Name, Operand)] -- Local scope symbol table
    , funcs :: [(Name, Operand)] -- Declared functions
    , externs :: [(Name, Operand)] -- Declared extern functions
  } deriving Show

newtype Codegen a = Codegen { runCodegen :: State CodegenState a }
  deriving (Functor, Applicative, Monad, MonadState CodegenState)

emptyCodegen :: CodegenState
emptyCodegen = CodegenState [] [] []

execCodegen :: Codegen a -> CodegenState
execCodegen m = execState (runCodegen m) emptyCodegen

evalCodegen :: Codegen a -> a
evalCodegen m = evalState (runCodegen m) emptyCodegen

type LLVMGen a = IRBuilderT (ModuleBuilderT Codegen) a

-------------------------------------------------------------------------------
-- Symbol Table
-------------------------------------------------------------------------------

-- | Associate a variable name with an operand in the symbol table
declvar :: Name -> Operand -> LLVMGen ()
declvar var x = do
  locals <- gets locals
  modify $ \s -> s { locals = (var, x) : locals }

getvar :: Name -> LLVMGen Operand
getvar var = do
  syms <- gets locals
  case lookup var syms of
    Just x  -> return x
    Nothing -> error $ "Local variable not in scope: " ++ show var


-- | Associate a function with an operand in the symbol table
declfunc :: Name -> Operand -> LLVMGen ()
declfunc var x = do
  funcs <- gets funcs
  modify $ \s -> s { funcs = (var, x) : funcs }

getfunc :: Name -> LLVMGen Operand
getfunc var = do
  syms <- gets funcs
  case lookup var syms of
    Just x  -> return x
    Nothing -> error $ "Function not in scope: " ++ show var

-- | Associate an external func with an operand in the symbol table
declextern :: Name -> Operand -> LLVMGen ()
declextern var x = do
  externs <- gets externs
  modify $ \s -> s { externs = (var, x) : externs}

getextern :: Name -> LLVMGen Operand
getextern var = do
  syms <- gets externs
  case lookup var syms of
    Just x  -> return x
    Nothing -> error $ "Extern func not in scope: " ++ show var

-- | Change a parac name into an LLVM name
tn2n :: T.Name -> Name
tn2n n = mkName (show n)

ntobs :: T.Name -> Data.ByteString.Short.Internal.ShortByteString
ntobs n = let (Name n') = tn2n n in n'

-- | Change a parac name into an LLVM name
qn2n :: T.QualifiedName -> Name
qn2n n = mkName (show n)

byte :: Applicative f => Integer -> f Operand
byte = pure . ConstantOperand . C.Int 8

word :: Applicative f => Integer -> f Operand
word = pure . ConstantOperand . C.Int 16


toLLVMType :: T.Type -> Type
toLLVMType (T.Int T.I1 T.Signed) = i1
toLLVMType (T.Int T.I1 T.Unsigned) = i1
toLLVMType (T.Int T.I8 T.Signed) = i8
toLLVMType (T.Int T.I8 T.Unsigned) = i8
toLLVMType (T.Int T.I16 T.Signed) = i16
toLLVMType (T.Int T.I16 T.Unsigned) = i16
toLLVMType (T.Int T.I32 T.Signed) = i32
toLLVMType (T.Int T.I32 T.Unsigned) = i32
toLLVMType (T.Int T.I64 T.Signed) = i64
toLLVMType (T.Int T.I64 T.Unsigned) = i64
toLLVMType (T.Int _ _) = i64
toLLVMType (T.Float T.F32) = float
toLLVMType (T.Float T.F64) = LLVM.AST.Type.double
toLLVMType (T.Float _) = LLVM.AST.Type.double
toLLVMType T.Void = VoidType
toLLVMType T.Char = i8
toLLVMType (T.Arr t (-1)) = ArrayType 0 (toLLVMType t)
toLLVMType (T.Arr t n) = ArrayType (fromIntegral n) (toLLVMType t)
toLLVMType T.Str =  ArrayType 0 i8
toLLVMType T.TUnspec = error "All types should be specified by this point"
toLLVMType (T.F _ _) = error "Function types not supported yet"
toLLVMType (T.List _) = error "List types not supported yet"


-- | Convert a parac binary operation into an llvm one for the given operand
-- types
bopToLLVMBop :: T.Type -> T.Type -> T.BinOp -> Operand -> Operand -> LLVMGen Operand
bopToLLVMBop (T.Int _ _) _ = \case
    T.Plus -> add
    T.Minus -> sub
    T.Times -> mul
    T.Div -> sdiv
    T.Lt -> icmp IP.SLT
    T.Lte -> icmp IP.SLE
    T.Gt -> icmp IP.SGT
    T.Gte -> icmp IP.SGE
    T.Eq -> icmp IP.EQ
    T.Neq -> icmp IP.NE
    T.Assign -> \o1 o2 -> do store o1 0 o2; return o1;
    a -> error $ "Operation " ++ show a ++ " not implemented for ints"
bopToLLVMBop (T.Char) _ = \case
    T.Plus -> add
    T.Minus -> sub
    T.Times -> mul
    T.Div -> sdiv
    T.Lt -> icmp IP.SLT
    T.Lte -> icmp IP.SLE
    T.Gt -> icmp IP.SGT
    T.Gte -> icmp IP.SGE
    T.Eq -> icmp IP.EQ
    T.Neq -> icmp IP.NE
    T.Assign -> \o1 o2 -> do store o1 0 o2; return o1;
    a -> error $ "Operation " ++ show a ++ " not implemented for ints"
bopToLLVMBop (T.Float _) _ = \case
    T.Plus -> fadd
    T.Minus -> fsub
    T.Times -> fmul
    T.Div -> fdiv
    T.Lt -> fcmp FP.OLT
    T.Lte -> fcmp FP.OLE
    T.Gt -> fcmp FP.OGT
    T.Gte -> fcmp FP.OGE
    T.Eq -> fcmp FP.OEQ
    T.Neq -> fcmp FP.ONE
    T.Assign -> \o1 o2 -> do store o1 0 o2; return o1;
    a -> error $ "Operation " ++ show a ++ " not implemented for floats"
bopToLLVMBop (T.Arr _ _) (T.Int _ _) = \case
  -- If it's an rval, we want the value
  T.ArrAccessR -> \arr idx -> do
    ptr <- gep arr (int64 0 ++ [idx])
    load ptr 0
  -- If it's an lval, we want the pointer
  T.ArrAccessL -> \arr idx -> gep arr (int64 0 ++ [idx])
  a -> error $ "Operation " ++ show a ++ " not implemented for arrs and ints"
bopToLLVMBop (T.Arr _ _) (T.Arr _ _) = \case
  T.Assign -> \to from-> do
      fptr <- gep from (int64 0)
      fval <- load fptr 0
      tptr <- gep to (int64 0)
      store tptr 0 fval
      return to
  a -> error $ "Operation " ++ show a ++ " not implemented for arrs"
bopToLLVMBop t1 t2 = \case
  a -> error $ "Operation " ++ show a ++ " not implemented for " ++ show t1 ++ " and " ++ show t2

uopToLLVMUop :: T.Type -> T.UnOp -> Operand -> LLVMGen Operand
uopToLLVMUop (T.Int _ _) = \case
  T.Neg -> undefined
  T.Not -> undefined
  T.Alloc -> undefined
  a -> error $ "Operation " ++ show a ++ " not implemented for arrs"
uopToLLVMUop (T.Float _) = \case
  T.Neg -> undefined
  T.Not -> undefined
  T.Alloc -> undefined
  a -> error $ "Operation " ++ show a ++ " not implemented for arrs"
uopToLLVMUop (T.Arr _ len) = \case
  T.Len -> return $ int64 (fromIntegral len)
  a -> error $ "Operation " ++ show a ++ " not implemented for arrs"
uopToLLVMUop t = \case
  a -> error $ "Operation " ++ show a ++ " not implemented for " ++ show t
