module Errors.InternalCompileError where
import Errors.CompileError

data InternalCompileError = InternalCompileError {
  message :: String
  }

instance CompileError InternalCompileError where
  toString (InternalCompileError message) = "Internal Compile Error: " ++ message
