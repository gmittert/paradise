{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Types where
import qualified Data.Map.Strict as M

data Type
  = Int
  | Bool
  | String Int
  | Char
  | Pointer Type
  | Arr Type Int
  deriving (Eq, Ord, Show)

data BinOp = Plus | Minus | Times | Div | Assign | Lt | Lte | Access
  deriving (Eq, Ord, Show)

data UnOp = Deref
  deriving (Eq, Ord, Show)

toSize :: Type -> Int
toSize Int = 8
toSize (Pointer _) = 8
toSize Bool = 1
toSize (String a) = a + 16
toSize Char = 1

newtype Name = Name {toString :: String}
  deriving (Eq, Ord)

instance Show Name where
  show = toString
