module Errors.WeederError where
import Ast.ParsedAst

data WeederError = WeederError {
  message :: String
  , stms :: [Statement]
  , exprs :: [Expr]
  }
  deriving (Eq, Ord)

instance Show WeederError where
  show (WeederError message stms exprs) = "Weeder Error:\n" ++ message ++ " while weeding " ++ concatMap show stms ++ concatMap show exprs
