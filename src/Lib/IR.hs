{- |
Module      : IR
Description : An intermediate representation based off of the Tree IR in Appel's
              Modern Compiler Implementation
Copyright   : (c) Jason Mittertreiner, 2017
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Lib.IR where
import Lib.Types
import Control.Monad.State.Lazy
import qualified Data.Map as M

data GenIRState = GenIRState {
  currFunc :: Name
  , nextLabel :: Int
  , nextTemp :: Int
}

emptyState :: GenIRState
emptyState = GenIRState (Name "" ) 0 0

newtype IRGen a = IRGen { irgen :: State GenIRState a }
  deriving (Functor, Applicative, Monad, MonadState GenIRState)

newLabel :: IRGen Label
newLabel = do
  modify $ \st -> st{nextLabel = nextLabel st + 1}
  st <- get
  return $ Label $ "L" ++ show (nextLabel st)

newTemp :: IRGen Exp
newTemp = do
  st <- get
  modify $ \st -> st{nextTemp = nextTemp st + 1}
  return $ Temp (nextTemp st)

setFunc :: Name -> IRGen ()
setFunc name = modify $ \st -> st{currFunc = name}


-- | Compute a value (with side effects)
data Exp
  -- | An integer constant
  = Const Int
  -- | A symbolic constant/label
  | EName Label
  -- | A temporary var
  | Temp Int
  -- | The ith argument
  | Arg Int
  -- | A special variable: the frame pointer
  | FP
  -- | Binary operation (note that we don't have unary operations)
  | Bop BinOp Exp Exp
  -- | Get the memory contents of exp
  | Mem Exp
  -- | Function call on the result of func
  | Call {func :: Exp, args:: [Exp]}
  -- | Function call on the result of func that is assigned to a var
  | ACall {func :: Exp, args:: [Exp]}
  -- | Evaluate Stm then Exp
  | Eseq Stm Exp
  deriving (Eq, Ord)
instance Show Exp where
  show (Const i) = "$" ++ show i
  show (EName l) = show l
  show (Temp i) = "t" ++ show i
  show (Lib.IR.Arg i) = "arg" ++ show i
  show FP = "FP"
  show (Bop op e1 e2) = show e1 ++ " " ++ show op ++ " " ++ show e2
  show (Mem e) = "Mem(" ++ show e ++ ")"
  show (Call f args) = show f ++ " " ++ show args
  show (ACall f args) = show f ++ " " ++ show args
  show (Eseq s e) = "Eseq " ++ show s ++ ", " ++ show e

-- | Operations that do side effects and control flow
data Stm
  -- | Evaluate e1 then e2. Move e2 into e1
  = Move {e1 :: Exp, e2 :: Exp}
  -- | Eval Exp and discard
  | Sexp Exp
  -- | Jump the the address at Exp, can be a literal label or computed address
  -- [Label] lists all possible locations Exp can evaluate to
  | Jump JumpTarget [Label]
  -- | Evaluate left, then right, the compare with relop and take the appropriate jump
  | Cjump {relop :: BinOp, left :: Exp, right :: Exp, iftrue:: Label, iffalse :: Label}
  -- | Eval s1 then s2
  | Seq {s1 :: Stm, s2 :: Stm}
  -- | Label definition/target of jumps
  | Lab Label
  -- | Designates the prologue of a function
  | FPro Name [Type] (M.Map Name Type)
  -- | Designates the epilogue of a function
  | FEpi Name [Type] (M.Map Name Type)
  deriving (Eq, Ord)
instance Show Stm where
  show (Move e1 e2) = show e1 ++ " <- " ++ show e2
  show (Sexp e) = show e
  show (Jump e _) = "Jump " ++ show e
  show (Cjump o l r t f) = "CJump " ++ show l ++ " " ++ show o ++ " " ++ show r ++ " " ++ show t ++ " " ++ show f
  show (Seq l r) = "Seq " ++ show l ++ "\n" ++ show r
  show (Lab l) = show l ++ ":"
  show (FPro n _ _) = "begin " ++ show n
  show (FEpi n _ _) = "end " ++ show n

data JumpTarget
  = Computed Exp
  | JLab Lib.Types.Label
  deriving (Eq, Ord)

instance Show JumpTarget where
  show (Computed e) = show e
  show (JLab l) = show l
