module Errors.CompileError where

class CompileError a where
  toString :: a -> String
