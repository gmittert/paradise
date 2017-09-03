module Lib.Types where

data Type
  = Int
  | Void
  | Bool
  | Char
  | Pointer Type
  | Arr Type Int
  | F Type [Type]
  deriving (Eq, Ord)
instance Show Type where
  show Int = "int"
  show Void = "void"
  show Bool = "bool"
  show Char = "char"
  show (Pointer t) = "*" ++ show t
  show (Arr t i) = show t ++ "[" ++ show i ++ "]"

data Def
  = FuncDef Type [Type]
  | VarDef Type
  deriving (Eq, Ord, Show)

data BinOp
  = Plus
  | Minus
  | Times
  | Div
  | Lt
  | Lte
  | Gt
  | Gte
  | Access
  | Eq
  | Neq
  deriving (Eq, Ord)
instance Show BinOp where
  show Plus = "+"
  show Minus = "-"
  show Times = "*"
  show Div = "/"
  show Lt = "<"
  show Lte = "<="
  show Gt = ">"
  show Gte = ">="
  show Neq = "!="
  show Eq = "=="
  show Access = "@"

data UnOp = Deref | Neg | Not
  deriving (Eq, Ord)
instance Show UnOp where
  show Deref = "*"
  show Neg = "-"
  show Not = "!"

toSize :: Type -> Int
toSize Int = 8
toSize (Pointer _) = 8
toSize Char = 1
toSize Void = 0
toSize Bool = 1
toSize (Arr tpe size) = size * toSize tpe

newtype Name = Name {toString :: String}
  deriving (Eq, Ord)

instance Show Name where
  show = toString

data Address
  -- | Globals and functions are addressed by labels
  = Fixed Name
  -- | Locals are addressed by an offset from the base pointer
  | Offset Int
  -- | Function arguments are given an argument count. This is later turned
  -- into either a register or offset from the base pointer depending on
  -- the number of arguments and calling conventions
  -- In the case of SystemV amd64, the first 6 args are passed in registers,
  -- (RDI, RSI, RDX, RCX, R8, R9) and the rest on the stack
  | Arg Int
  deriving (Eq, Ord, Show)

newtype Label = Label{label :: String}
  deriving (Eq, Ord)
instance Show Label where
  show = label

-- | Get the Label indicating the beginning the prologue of a function
funcBegin :: Name -> Label
funcBegin name = Label ("func__" ++ show name)

-- | Get the Label indicating the beginning of the epilogue of a function
funcEnd :: Name -> Label
funcEnd name = Label (show name ++ "__end")
