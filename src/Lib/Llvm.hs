{- |
Module      : GenLLVM
Description : Generate the llvm code for the program
Copyright   : Modified by Jason Mittertreiner, 2017
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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
module Lib.Llvm where

import Data.List
import Data.Function
import qualified Data.Map as Map

import Control.Monad.State

import LLVM.AST
import LLVM.AST.Type
import LLVM.AST.Global
import qualified LLVM.AST as AST

import qualified LLVM.AST.Linkage as L
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Attribute as A
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.FloatingPointPredicate as FP
import qualified LLVM.AST.IntegerPredicate as IP

import qualified Lib.Types as T
-------------------------------------------------------------------------------
-- Module Level
-------------------------------------------------------------------------------

newtype LLVM a = LLVM (State AST.Module a)
  deriving (Functor, Applicative, Monad, MonadState AST.Module )

runLLVM :: AST.Module -> LLVM a -> AST.Module
runLLVM mod (LLVM m) = execState m mod

emptyModule :: Name -> AST.Module
emptyModule (Name label) = defaultModule {
  moduleName = label
  , moduleTargetTriple = Just "x86_64-pc-linux-gnu"
  }

addDefn :: Definition -> LLVM ()
addDefn d = do
  defs <- gets moduleDefinitions
  modify $ \s -> s { moduleDefinitions = defs ++ [d] }

function ::  Type -> Name -> [(Type, Name)] -> [BasicBlock] -> LLVM ()
function retty label argtys body = addDefn $
  GlobalDefinition $ functionDefaults {
    name        = label
  , parameters  = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
  , returnType  = retty
  , basicBlocks = body
  }

external ::  Type -> Name -> [(Type, Name)] -> LLVM ()
external retty label argtys = addDefn $
  GlobalDefinition $ functionDefaults {
    name        = label
  , linkage     = L.External
  , parameters  = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
  , returnType  = retty
  , basicBlocks = []
  }

-------------------------------------------------------------------------------
-- Names
-------------------------------------------------------------------------------

type Names = Map.Map Name Int

uniqueName :: Name -> Names -> (Name, Names)
uniqueName nm@(Name bs) ns =
  case Map.lookup nm ns of
    Nothing -> (nm,  Map.insert nm 1 ns)
    Just ix -> (Name (bs `mappend` read (show ix)), Map.insert nm (ix+1) ns)

-------------------------------------------------------------------------------
-- Codegen State
-------------------------------------------------------------------------------

type SymbolTable = [(Name, Operand)]

data CodegenState
  = CodegenState {
    currentBlock :: Name                     -- Name of the active block to append to
  , blocks       :: Map.Map Name BlockState  -- Blocks for function
  , symtab       :: SymbolTable              -- Function scope symbol table
  , blockCount   :: Int                      -- Count of basic blocks
  , count        :: Word                     -- Count of unnamed instructions
  , names        :: Names                    -- Name Supply
  } deriving Show

data BlockState
  = BlockState {
    idx   :: Int                            -- Block index
  , stack :: [Named Instruction]            -- Stack of instructions
  , term  :: Maybe (Named Terminator)       -- Block terminator
  } deriving Show

-------------------------------------------------------------------------------
-- Codegen Operations
-------------------------------------------------------------------------------

newtype Codegen a = Codegen { runCodegen :: State CodegenState a }
  deriving (Functor, Applicative, Monad, MonadState CodegenState )

sortBlocks :: [(Name, BlockState)] -> [(Name, BlockState)]
sortBlocks = sortBy (compare `on` (idx . snd))

createBlocks :: CodegenState -> [BasicBlock]
createBlocks m = map makeBlock $ sortBlocks $ Map.toList (blocks m)

makeBlock :: (Name, BlockState) -> BasicBlock
makeBlock (l, BlockState _ s t) = BasicBlock l (reverse s) (maketerm t)
  where
    maketerm (Just x) = x
    maketerm Nothing = error $ "Block has no terminator: " ++ show l

entryBlockName :: Name
entryBlockName = mkName "entry"

emptyBlock :: Int -> BlockState
emptyBlock i = BlockState i [] Nothing

emptyCodegen :: CodegenState
emptyCodegen = CodegenState entryBlockName Map.empty [] 1 0 Map.empty

execCodegen :: Codegen a -> CodegenState
execCodegen m = execState (runCodegen m) emptyCodegen

fresh :: Codegen Word
fresh = do
  i <- gets count
  modify $ \s -> s { count = 1 + i }
  return $ i + 1

instr :: Instruction -> Type -> Codegen Operand
instr ins tpe = do
  n <- fresh
  let ref = UnName n
  blk <- current
  -- Add our new instruction to the stack
  let stack' = (ref := ins) : stack blk
  modifyBlock (blk { stack = stack' } )
  return $ local tpe ref

-- Add a new instruction of void type
vinstr :: Instruction -> Codegen ()
vinstr ins = do
  blk <- current
  -- Add our new instruction to the stack
  let stack' = (Do ins) : stack blk
  modifyBlock (blk { stack = stack' } )

terminator :: Named Terminator -> Codegen (Named Terminator)
terminator trm = do
  blk <- current
  modifyBlock (blk { term = Just trm })
  return trm

-------------------------------------------------------------------------------
-- Block Stack
-------------------------------------------------------------------------------

entry :: Codegen Name
entry = gets currentBlock

addBlock :: Name -> Codegen Name
addBlock bname = do
  bls <- gets blocks
  ix  <- gets blockCount
  nms <- gets names

  let new = emptyBlock ix
      (qname, supply) = uniqueName bname nms

  modify $ \s -> s { blocks = Map.insert qname new bls
                   , blockCount = ix + 1
                   , names = supply
                   }
  return qname

setBlock :: Name -> Codegen Name
setBlock bname = do
  modify $ \s -> s { currentBlock = bname }
  return bname

getBlock :: Codegen Name
getBlock = gets currentBlock

modifyBlock :: BlockState -> Codegen ()
modifyBlock new = do
  active <- gets currentBlock
  modify $ \s -> s { blocks = Map.insert active new (blocks s) }

current :: Codegen BlockState
current = do
  c <- gets currentBlock
  blks <- gets blocks
  case Map.lookup c blks of
    Just x -> return x
    Nothing -> error $ "No such block: " ++ show c

-------------------------------------------------------------------------------
-- Symbol Table
-------------------------------------------------------------------------------

-- | Associate a variable name with an operand in the symbol table
declVar :: Name -> Operand -> Codegen ()
declVar var x = do
  locals <- gets symtab
  modify $ \s -> s { symtab = (var, x) : locals }

getvar :: Name -> Codegen Operand
getvar var = do
  syms <- gets symtab
  case lookup var syms of
    Just x  -> return x
    Nothing -> error $ "Local variable not in scope: " ++ show var

-------------------------------------------------------------------------------

-- References
local ::  Type -> Name -> Operand
local = LocalReference

global ::  Type -> Name -> C.Constant
global = C.GlobalReference

externf :: Type -> Name -> Operand
externf t = ConstantOperand . C.GlobalReference t

-- Arithmetic operations
--
-- Signed and unsigned wrapping is disabled
add :: Type -> Operand -> Operand  -> Codegen Operand
add tpe a b = instr (Add True True a b []) tpe

fadd :: Type -> Operand -> Operand  -> Codegen Operand
fadd tpe a b = instr (FAdd noFastMathFlags a b []) tpe

sub :: Type -> Operand -> Operand  -> Codegen Operand
sub tpe a b = instr (Sub True True a b []) tpe

fsub :: Type -> Operand -> Operand  -> Codegen Operand
fsub tpe a b = instr (FSub noFastMathFlags a b []) tpe

mul :: Type -> Operand -> Operand  -> Codegen Operand
mul tpe a b = instr (Mul True True a b []) tpe

fmul :: Type -> Operand -> Operand  -> Codegen Operand
fmul tpe a b = instr (FMul noFastMathFlags a b []) tpe

sdiv :: Type -> Operand -> Operand  -> Codegen Operand
sdiv tpe a b = instr (SDiv False a b []) tpe

fdiv :: Type -> Operand -> Operand  -> Codegen Operand
fdiv tpe a b = instr (FDiv noFastMathFlags a b []) tpe

cmp :: Type -> IP.IntegerPredicate -> Operand -> Operand -> Codegen Operand
cmp tpe cond a b = instr (ICmp cond a b []) tpe

fcmp :: Type -> FP.FloatingPointPredicate -> Operand -> Operand -> Codegen Operand
fcmp tpe cond a b = instr (FCmp cond a b []) tpe

constOp :: C.Constant -> Operand
constOp = ConstantOperand

uitofp :: Type -> Operand -> Type -> Codegen Operand
uitofp ty a tpe = instr (UIToFP a ty []) tpe

toArgs :: [Operand] -> [(Operand, [A.ParameterAttribute])]
toArgs = map (\x -> (x, []))

-- Effects
call :: Operand -> [Operand] -> Type -> Codegen Operand
call fn args tpe = instr (Call Nothing CC.C [] (Right fn) (toArgs args) [] []) tpe

alloca :: Type -> Codegen Operand
alloca tpe = instr (Alloca tpe Nothing 0 []) tpe

store :: Operand -> Operand -> Codegen ()
store ptr val = vinstr (Store False ptr val Nothing 0 [])

load :: Operand -> Type -> Codegen Operand
load ptr tpe = instr (Load False ptr Nothing 0 []) tpe

-- Control Flow
br :: Name -> Codegen (Named Terminator)
br val = terminator $ Do $ Br val []

cbr :: Operand -> Name -> Name -> Codegen (Named Terminator)
cbr cond tr fl = terminator $ Do $ CondBr cond tr fl []

ret :: Operand -> Codegen (Named Terminator)
ret val = terminator $ Do $ Ret (Just val) []


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
toLLVMType (T.Int _ _) = error "All ints should be fully specified by this point"
toLLVMType (T.Float T.F32) = float
toLLVMType (T.Float T.F64) = double
toLLVMType (T.Float _) = error "All float should be fully specified by this point"
toLLVMType T.Void = VoidType
toLLVMType T.Char = i8
-- TODO: It shouldn't be hard to determine array length and properly set it here
toLLVMType (T.Arr t) = ArrayType 0 (toLLVMType t)
toLLVMType T.Str =  ArrayType 0 i8
toLLVMType T.TUnspec = error "All types should be specified by this point"
toLLVMType (T.F _ _) = error "Function types not supported yet"


bopToLLVMBop :: T.Type -> T.BinOp -> Operand -> Operand -> Codegen Operand
bopToLLVMBop t@(T.Int _ _) = let t' = toLLVMType t in \case
    T.Plus -> add t'
    T.Minus -> sub t'
    T.Times -> mul t'
    T.Div -> sdiv t'
    T.Lt -> cmp t' IP.SLT
    T.Lte -> cmp t' IP.SLE
    T.Gt -> cmp t' IP.SGT
    T.Gte -> cmp t' IP.SGE
    T.Eq -> cmp t' IP.EQ
    T.Neq -> cmp t' IP.NE
bopToLLVMBop t@(T.Float _) = let t' = toLLVMType t in \case
    T.Plus -> fadd t'
    T.Minus -> fsub t'
    T.Times -> fmul t'
    T.Div -> fdiv t'
    T.Lt -> fcmp t' FP.OLT
    T.Lte -> fcmp t' FP.OLE
    T.Gt -> fcmp t' FP.OGT
    T.Gte -> fcmp t' FP.OGE
    T.Eq -> fcmp t' FP.OEQ
    T.Neq -> fcmp t' FP.ONE

-- | Get a variable reference from the symbol table
varToOp :: T.Name -> Codegen AST.Operand
varToOp (T.Name n) = do
  syms <- gets symtab
  case lookup (mkName n) syms of
    Just x  -> return x
    Nothing -> error $ "Local variable not in scope: " ++ show n

uopToLLVMUop :: T.Type -> T.UnOp -> Operand -> Codegen Operand
uopToLLVMUop (T.Int _ _) = \case
  T.Len -> undefined
  T.Neg -> undefined
  T.Not -> undefined
  T.Alloc -> undefined
uopToLLVMUop (T.Float _) = \case
  T.Len -> undefined
  T.Neg -> undefined
  T.Not -> undefined
  T.Alloc -> undefined
