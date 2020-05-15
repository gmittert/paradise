{- |
Module      : GenLLVM
Description : Generate the llvm code for the program
Copyright   : Modified by Gwern Mittertreiner, 2017
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Lib.Llvm where

import Control.Monad.State

import LLVM.AST
import qualified LLVM.AST.Constant as C
import LLVM.AST.Global
import LLVM.AST.Linkage
import LLVM.AST.Type
import LLVM.IRBuilder.Constant
import LLVM.IRBuilder.Instruction
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad

import qualified LLVM.AST.FloatingPointPredicate as FP
import qualified LLVM.AST.IntegerPredicate as IP

import Data.ByteString.Short.Internal
import qualified Lib.Types as T
import qualified Ast.OpenCLAst as CLA
import qualified Lib.SymbolTable as ST

-------------------------------------------------------------------------------
-- Codegen State
-------------------------------------------------------------------------------
data CodegenState = CodegenState
  { locals :: [(Name, Operand)] -- Local scope operand symbol table
  , params :: [(Name, Operand)] -- Function parameters symbol table
  , funcs :: [(Name, Operand)] -- Declared functions
  , externs :: [(Name, Operand)] -- Declared extern functions
  , kernels :: [CLA.Kernel] -- The kernels that we've created
  , symTab :: ST.SymbolTable -- SymbolTable for type information
  } deriving (Show)

newtype Codegen a = Codegen
  { doCodegen :: State CodegenState a
  } deriving (Functor, Applicative, Monad, MonadState CodegenState)

emptyCodegen :: CodegenState
emptyCodegen = CodegenState [] [] [] [] [] ST.emptyTable

execCodegen :: ST.SymbolTable -> Codegen a -> CodegenState
execCodegen t = snd . runCodegen t

evalCodegen :: ST.SymbolTable -> Codegen a -> a
evalCodegen t = fst . runCodegen t

runCodegen :: ST.SymbolTable -> Codegen a -> (a, CodegenState)
runCodegen t m = runState (doCodegen m) emptyCodegen{symTab = t}

type LLVMGen a = IRBuilderT (ModuleBuilderT Codegen) a

-------------------------------------------------------------------------------
-- Symbol Table
-------------------------------------------------------------------------------
-- | Associate a variable name with an operand in the symbol table
declvar :: Name -> Operand -> Codegen ()
declvar var x = do
  locals <- gets locals
  modify $ \s -> s {locals = (var, x) : locals}

getvar :: Name -> Codegen Operand
getvar var = do
  syms <- gets locals
  case lookup var syms of
    Just x -> return x
    Nothing ->
      error $ concat ["Local variable ", show var, " not in scope: ", show syms]

isVarDefined :: Name -> Codegen Bool
isVarDefined var = do
  syms <- gets locals
  return $
    case lookup var syms of
      Just _ -> True
      Nothing -> False

declParams :: [(Name, Operand)] -> Codegen ()
declParams p = modify $ \s -> s {params = p}

getparam :: Name -> Codegen Operand
getparam var = do
  syms <- gets params
  case lookup var syms of
    Just x -> return x
    Nothing -> error $ "Parameter not in scope: " ++ show var ++ "\n\n" ++ show syms

-- | Associate a function with an operand in the symbol table
declFunc :: Name -> Operand -> Codegen ()
declFunc var x = do
  funcs <- gets funcs
  modify $ \s -> s {funcs = (var, x) : funcs}

getfunc :: Name -> Codegen Operand
getfunc var = do
  syms <- gets funcs
  case lookup var syms of
    Just x -> return x
    Nothing -> error $ "Function not in scope: " ++ show var

mkFuncRef :: Name -> T.Def -> Operand
mkFuncRef n (T.FuncDef t args) = let
  funty = ptr $ FunctionType (toLLVMType t) (map toLLVMType args) False
  in ConstantOperand $ C.GlobalReference funty n
mkFuncRef n (T.CDef (T.CFunc _ t args)) = let
  isVarArgs = elem T.Varargs args
  args' = if isVarArgs then init args else args
  funty = ptr $ FunctionType (toLLVMType t) (map toLLVMType args') isVarArgs
  in ConstantOperand $ C.GlobalReference funty n
mkFuncRef _ a = error $ "Cannot make function ref out of " ++ show a

-- | Associate an external func with an operand in the symbol table
declextern :: Name -> Operand -> Codegen ()
declextern var x = do
  externs <- gets externs
  modify $ \s -> s {externs = (var, x) : externs}

getextern :: Name -> Codegen Operand
getextern var = do
  syms <- gets externs
  case lookup var syms of
    Just x -> return x
    Nothing -> error $ "Extern func not in scope: " ++ show var

-- | Change a parac name into an LLVM name
tn2n :: T.Name -> Name
tn2n n = mkName (show n)

ntobs :: T.Name -> Data.ByteString.Short.Internal.ShortByteString
ntobs n =
  let (Name n') = tn2n n
   in n'

-- | Change a parac name into an LLVM name
qn2n :: T.QualifiedName -> Name
qn2n n = mkName (show n)

byte :: Integer -> Operand
byte = ConstantOperand . C.Int 8

word :: Integer -> Operand
word = ConstantOperand . C.Int 16

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
toLLVMType (T.Arr t (-1)) = toLLVMType (T.Arr t 0)
toLLVMType (T.Arr t n) =
  -- We represent arrays as a struct of a raw array and its length
  let arrtpe = ArrayType (fromIntegral n) (toLLVMType t)
      lentpe = i64
   in StructureType False [lentpe, arrtpe]
toLLVMType (T.Str l) = toLLVMType (T.Arr T.Char (fromIntegral l))
toLLVMType (T.F _ _) = error "Function types not supported yet"
toLLVMType (T.List _) = error "List types not supported yet"
toLLVMType T.Varargs = error "Varargs should be removed in typer"
toLLVMType (T.Ptr t) = ptr (toLLVMType t)
toLLVMType (T.UserType _ ) = ptr i64
toLLVMType (T.TypeVar _) = ptr i64

-- | We box all types that don't fit in a register
box :: Type -> Type
-- When we box an array, we drop the length information
box (StructureType False [IntegerType 64, ArrayType _ d]) = ptr (StructureType False [IntegerType 64, ArrayType 0 d])
box (StructureType a b) = ptr (StructureType a b)
box a = a

-- | Removes the specified length from an array type
removeLen :: Type -> Type
removeLen (StructureType False [IntegerType 64, ArrayType _ d]) = StructureType False [IntegerType 64, ArrayType 0 d]
removeLen _ = error "This isn't an array"

-- | Convert a parac binary operation into an llvm one for the given operand
-- types
bopToLLVMBop ::
     T.Type -> T.Type -> T.BinOp -> Operand -> Operand -> LLVMGen Operand
bopToLLVMBop (T.Int _ _) _ =
  \case
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
    T.Assign ->
      \o1 o2 -> do
        store o1 0 o2
        return o1
    a -> error $ "Operation " ++ show a ++ " not implemented for ints"
bopToLLVMBop T.Char _ =
  \case
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
    T.Assign ->
      \o1 o2 -> do
        store o1 0 o2
        return o1
    a -> error $ "Operation " ++ show a ++ " not implemented for ints"
bopToLLVMBop (T.Float _) _ =
  \case
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
    T.Assign ->
      \o1 o2 -> do
        store o1 0 o2
        return o1
    a -> error $ "Operation " ++ show a ++ " not implemented for floats"
bopToLLVMBop (T.Arr _ _) (T.Int _ _)
  -- If it's an rval, we want the value
 =
  \case
    T.ArrAccessR ->
      \arr idx -> do
        -- t <- load arr 0
        ptr <- gep arr (int32 0 : int32 1 : [idx])
        load ptr 0
  -- If it's an lval, we want the pointer
    T.ArrAccessL -> \arr idx -> gep arr (int32 0 : int32 1 : [idx])
    a -> error $ "Operation " ++ show a ++ " not implemented for arrs and ints"
bopToLLVMBop (T.Arr _ _) (T.Arr _ _) =
  \case
    T.Assign ->
      \o1 o2 -> do
        store o1 0 o2
        return o1
    a -> error $ "Operation " ++ show a ++ " not implemented "
bopToLLVMBop t1 t2
  | t1 == t2 = \case
      T.Assign -> \o1 o2 -> do
        store o1 0 o2
        return o1
      a -> error $ "Operation " ++ show a ++ " not implemented for " ++ show t1 ++ " and " ++ show t2
  | otherwise =
    \case
      a ->
        error $
        "Operation " ++
        show a ++ " not implemented for " ++ show t1 ++ " and " ++ show t2

uopToLLVMUop :: T.Type -> T.UnOp -> Operand -> LLVMGen Operand
uopToLLVMUop (T.Int _ _) =
  \case
    T.Neg -> undefined
    T.Not -> undefined
    a -> error $ "Operation " ++ show a ++ " not implemented for arrs"
uopToLLVMUop (T.Float _) =
  \case
    T.Neg -> undefined
    T.Not -> undefined
    a -> error $ "Operation " ++ show a ++ " not implemented for arrs"
-- If we can't figure out the length at compile time, we have to get it at
-- runtime instead
uopToLLVMUop (T.Arr _ (-1)) =
  \case
    T.Len -> \arr -> do
      ptr <- gep arr [int32 0, int32 0]
      load ptr 0
    a -> error $ "Operation " ++ show a ++ " not implemented for arrs"
uopToLLVMUop (T.Arr _ len) =
  \case
    T.Len -> \_-> return $ int64 (fromIntegral len)
    a -> error $ "Operation " ++ show a ++ " not implemented for arrs"
uopToLLVMUop (T.Str (-1)) =
  \case
    T.Len -> \arr -> do
      ptr <- gep arr [int32 0, int32 0]
      load ptr 0
    a -> error $ "Operation " ++ show a ++ " not implemented for arrs"
uopToLLVMUop (T.Str len) =
  \case
    T.Len -> \_ -> return $ int64 (fromIntegral len)
    a -> error $ "Operation " ++ show a ++ " not implemented for arrs"
uopToLLVMUop t =
  \case
    a -> error $ "Operation " ++ show a ++ " not implemented for " ++ show t

 -- | Define and emit a (non-variadic) function definition
function
  :: MonadModuleBuilder m
  => Name  -- ^ Function name
  -> [(Type, ParameterName)]  -- ^ Parameter types and name suggestions
  -> Type  -- ^ Return type
  -> ([Operand] -> IRBuilderT m ())  -- ^ Function body builder
  -> m Operand
function label argtys retty body = do
  let tys = fst <$> argtys
  (paramNames, blocks) <- runIRBuilderT emptyIRBuilder $ do
    paramNames <- forM argtys $ \(_, paramName) -> case paramName of
      NoParameterName -> fresh
      ParameterName p -> fresh `named` p
    body $ zipWith LocalReference tys paramNames
    return paramNames
  let
    def = GlobalDefinition functionDefaults
      { name        = label
      , parameters  = (zipWith (\ty nm -> Parameter ty nm []) tys paramNames, False)
      , returnType  = retty
      , basicBlocks = blocks
      , garbageCollectorName = Just "statepoint-example"
      }
    funty = ptr $ FunctionType retty (fst <$> argtys) False
  emitDefn def
  pure $ ConstantOperand $ C.GlobalReference funty label 

-- | An external function definition
extern ::
     MonadModuleBuilder m
  => Name -- ^ Definition name
  -> [Type] -- ^ Parameter types
  -> Type -- ^ Type
  -> Bool -- ^ Is Varargs
  -> m Operand
extern nm argtys retty isVarArgs = do
  emitDefn $
    GlobalDefinition
      functionDefaults
        { name = nm
        , linkage = External
        , parameters = ([Parameter ty (mkName "") [] | ty <- argtys], isVarArgs)
        , returnType = retty
        }
  let funty = ptr $ FunctionType retty argtys isVarArgs
  pure $ ConstantOperand $ C.GlobalReference funty nm

-- | Return true if the operand is of the constructor type
checkCtor :: Operand -> Operand -> LLVMGen Operand
checkCtor exp tag = do
  tagptr <- gep exp [int32 0, int32 0]
  tagval <- load tagptr 0
  icmp IP.EQ tagval tag



