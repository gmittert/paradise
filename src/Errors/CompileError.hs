module Errors.CompileError where

import Errors.InternalCompileError
import Errors.ResolverError
import Errors.TypeError
import Errors.WeederError
import Errors.ImporterError

data CompileError
  = InternalCompileE InternalCompileError
  | ResolverE ResolverError
  | WeederE WeederError
  | TyperE TypeError
  | ImporterE ImporterError
  deriving (Eq, Ord)

instance Show CompileError where
  show (InternalCompileE e) = show e
  show (ResolverE e) = show e
  show (WeederE e) = show e
  show (TyperE e) = show e
  show (ImporterE e) = show e
