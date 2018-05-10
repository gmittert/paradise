module Errors.ResolverError where
import Ast.WeededAst

data ResolverError = ResolverError {
  message :: String
  , stms :: [Statement]
  , exprs :: [Expr]
  }
  deriving (Eq, Ord)

instance Show ResolverError where
  show (ResolverError message stms exprs) = "Resolver Error:\n" ++ message ++ " while resolving " ++ concatMap show stms ++ concatMap show exprs
