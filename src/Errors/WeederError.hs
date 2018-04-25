module Errors.WeederError where
import Errors.CompileError
import Ast.ParsedAst

data WeederError = WeederError {
  message :: String
  , stms :: [Statement]
  , exprs :: [Expr]
  }

instance CompileError WeederError where
  toString (WeederError message stms exprs) = "Weeder Error:\n" ++ message ++ " while weeding " ++ concatMap show stms ++ concatMap show exprs
