module Lib.IR where
import Types

newtype Label = Label{label :: String}
  deriving (Eq, Ord)
instance Show Label where
  show = label
newtype Var = Var{var :: String}
  deriving (Eq, Ord)
instance Show Var where
  show = var

data IRLVal
  = IRChar Char
  | IRInt Int
  | IRVar Var
  | IRUOp UnOp Var
  | IRBOp BinOp Var Var
  deriving (Eq, Ord)
instance Show IRLVal where
  show (IRChar c) = show c
  show (IRInt i) = show i
  show (IRVar v) = show v
  show (IRUOp op v) = show op ++ " " ++ show v
  show (IRBOp op v1 v2) = show v1 ++ " " ++  show op ++ " " ++ show v2

data Instr
  -- Assignment: Name = Lval
  = Assign Var IRLVal
  -- Unconditional Jump goto Label
  | Goto Label
  -- Conditional Jump: If Var is zero, goto Int
  | BrZero Var Label
  | Lab Label
  | Ret Var
  deriving (Eq, Ord)
instance Show Instr where
  show (Assign v lval) = show v ++ " = " ++ show lval ++ "\n"
  show (Goto l) = "br " ++ show l ++ "\n"
  show (BrZero v l) = "br0 " ++ show v ++ " " ++ show l ++ "\n"
  show (Lab l) = show l ++ ": " ++ "\n"
  show (Ret v) = "ret " ++ show v ++ "\n"

isAssign :: Instr -> Bool
isAssign Assign{} = True
isAssign _ = False

getRVal :: Instr -> Var
getRVal (Assign v _) = v
