module Lib.IR where
import Lib.Types

newtype Label = Label{label :: String}
  deriving (Eq, Ord)
instance Show Label where
  show = label
data Var = Var String Type
  deriving (Eq, Ord)
instance Show Var where
  show (Var s _)= s -- ++ ": " ++ show tpe

data RVal
  = RInt Int
  -- A variable, its name and its offset
  | IRVar Var
  | IRArr Var Type
  | IRUOp UnOp Var
  | IRBOp BinOp Var Var
  | Call Label [Var]
  deriving (Eq, Ord)

instance Show RVal where
  show (RInt i) = show i
  show (IRArr _ t) = show t
  show (IRVar v) = show v
  show (IRUOp op v) = show op ++ " " ++ show v
  show (IRBOp op v1 v2) = show v1 ++ " " ++  show op ++ " " ++ show v2
  show (Call l args) = show l ++ " " ++ show args

data LVal
  = LVar Var
  | LAccess Var Var
  deriving (Eq, Ord)

instance Show LVal where
  show (LVar v) = show v
  show (LAccess v i) = show v ++ "[" ++ show i ++ "]"

data Instr
  -- Assignment: Name = Lval
  = Assign LVal RVal
  -- Unconditional Jump goto Label
  | Goto Label
  -- Conditional Jump: If Var is zero, goto Int
  | BrZero Var Label
  -- An unexported label
  | Lab Label
  | Func Name [(Type, Name)]
  | Ret Var
  deriving (Eq, Ord)
instance Show Instr where
  show (Assign lval rval) = show rval ++ " = " ++ show lval ++ "\n"
  show (Func name _) = "func " ++ show name ++ "\n"
  show (Goto l) = "br " ++ show l ++ "\n"
  show (BrZero v l) = "br0 " ++ show v ++ " " ++ show l ++ "\n"
  show (Lab l) = show l ++ ": " ++ "\n"
  show (Ret v) = "ret " ++ show v ++ "\n"

isAssign :: Instr -> Bool
isAssign Assign{} = True
isAssign _ = False

getLVal :: Instr -> LVal
getLVal (Assign l _) = l
