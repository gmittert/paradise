module Errors.CompileError where

import Errors.InternalCompileError
import Errors.ResolverError
import Errors.TypeError
import Errors.WeederError
import Errors.ImporterError
import Errors.ParseError
import qualified Ast.WeededAst as WA

data CompileError
  = InternalCompileE InternalCompileError
  | ResolverE ResolverError
  | WeederE WeederError
  | TyperE TypeError
  | ImporterE ImporterError
  | ParseE ParseError
  deriving (Eq, Ord)

throwInternComp :: String -> CompileError
throwInternComp = InternalCompileE . InternalCompileError

mkResolverE :: String -> [WA.Statement] -> [WA.Expr] -> CompileError
mkResolverE s stms exprs = ResolverE $ ResolverError s stms exprs

instance Show CompileError where
  show (InternalCompileE e) = show e
  show (ParseE e) = show e
  show (ResolverE e) = show e
  show (WeederE e) = show e
  show (TyperE e) = show e
  show (ImporterE e) = show e
