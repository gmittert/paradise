module Types where

newtype Name = Name {toString :: String}
  deriving (Eq, Ord)

instance Show Name where
  show = toString
