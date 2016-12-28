module Types where

newtype Name = Name {toString :: String}
  deriving (Eq, Ord, Show)
