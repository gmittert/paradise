module Lib.IR where
import Types

newtype Label = Label{label :: String}
newtype Var = Var{var :: String}

data IRLVal
  = IRChar Char
  | IRInt Int
  | IRVar Var
  | IRUOp UnOp Var
  | IRBOp BinOp Var Var
  | IRNop

data IRInstr
  -- Assignment: Name = Lval
  = IRAssign Var IRLVal
  -- Unconditional Jump goto Label
  | IRGoto Label
  -- Conditional Jump: If Var is zero, goto Int
  | IRBrZero Var Label
  | IRLabel Label
