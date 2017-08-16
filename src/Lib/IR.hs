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
  | IRNop
  deriving (Eq, Ord, Show)

data IRInstr
  -- Assignment: Name = Lval
  = IRAssign Var IRLVal
  -- Unconditional Jump goto Label
  | IRGoto Label
  -- Conditional Jump: If Var is zero, goto Int
  | IRBrZero Var Label
  | IRLabel Label
  deriving (Eq, Ord, Show)
