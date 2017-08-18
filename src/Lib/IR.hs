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

data IRInstr
  -- Assignment: Name = Lval
  = IRAssign Var IRLVal
  -- Unconditional Jump goto Label
  | IRGoto Label
  -- Conditional Jump: If Var is zero, goto Int
  | IRBrZero Var Label
  | IRLabel Label
  deriving (Eq, Ord)
instance Show IRInstr where
  show (IRAssign v lval) = show v ++ " = " ++ show lval ++ "\n"
  show (IRGoto l) = "br " ++ show l ++ "\n"
  show (IRBrZero v l) = "br0 " ++ show v ++ " " ++ show l ++ "\n"
  show (IRLabel l) = show l ++ ": " ++ "\n"

isAssign :: IRInstr -> Bool
isAssign IRAssign{} = True
isAssign _ = False

getRVal :: IRInstr -> Var
getRVal (IRAssign v _) = v
