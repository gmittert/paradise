module Lib.IR where
import Types

newtype Label = Label{label :: String}
  deriving (Eq, Ord)
instance Show Label where
  show = label
data Var = Var String Type
  deriving (Eq, Ord)
instance Show Var where
  show (Var s _)= s -- ++ ": " ++ show tpe

data LVal
  = LInt Int
  -- A variable, its name and its offset
  | IRVar Var
  | IRArr Var Type
  | IRUOp UnOp Var
  | IRBOp BinOp Var Var
  deriving (Eq, Ord)

instance Show LVal where
  show (LInt i) = show i
  show (IRArr _ t) = show t
  show (IRVar v) = show v
  show (IRUOp op v) = show op ++ " " ++ show v
  show (IRBOp op v1 v2) = show v1 ++ " " ++  show op ++ " " ++ show v2

data RVal
  = RVar Var
  | RAccess Var Var
  deriving (Eq, Ord)

instance Show RVal where
  show (RVar v) = show v
  show (RAccess v i) = show v ++ "[" ++ show i ++ "]"

data Instr
  -- Assignment: Name = Lval
  = Assign RVal LVal
  -- Unconditional Jump goto Label
  | Goto Label
  -- Conditional Jump: If Var is zero, goto Int
  | BrZero Var Label
  -- An unexported label
  | Lab Label
  | Func Name
  | Call Label [LVal]
  | Ret Var
  deriving (Eq, Ord)
instance Show Instr where
  show (Assign rval lval) = show rval ++ " = " ++ show lval ++ "\n"
  show (Func name) = "func " ++ show name ++ "\n"
  show (Call l args) = show l ++ " " ++ show args ++ "\n"
  show (Goto l) = "br " ++ show l ++ "\n"
  show (BrZero v l) = "br0 " ++ show v ++ " " ++ show l ++ "\n"
  show (Lab l) = show l ++ ": " ++ "\n"
  show (Ret v) = "ret " ++ show v ++ "\n"

isAssign :: Instr -> Bool
isAssign Assign{} = True
isAssign _ = False

getRVal :: Instr -> RVal
getRVal (Assign r _) = r
