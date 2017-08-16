module Types where

data Type
  = Int
  | Void
  | Bool
  | Char
  | Pointer Type
  | Arr Type Int
  deriving (Eq, Ord, Show)

data BinOp = Plus | Minus | Times | Div | Lt | Lte | Access
  deriving (Eq, Ord, Show)

data UnOp = Deref | Neg
  deriving (Eq, Ord, Show)

toSize :: Type -> Int
toSize Int = 8
toSize (Pointer _) = 8
toSize Char = 1
toSize Void = 0
toSize Bool = 1

newtype Name = Name {toString :: String}
  deriving (Eq, Ord)

instance Show Name where
  show = toString
