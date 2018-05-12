module Errors.InternalCompileError where

-- An error that is a result of an error in the parac compiler logic
newtype InternalCompileError = InternalCompileError {message :: String}
  deriving (Eq, Ord)

instance Show InternalCompileError where
  show (InternalCompileError message) = "Internal Compile Error:\n" ++ message
