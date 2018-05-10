module Errors.TypeError where
import qualified Ast.TypedAst as TA

data TypeError = TypeError {
  message :: [String],
  exprs :: [TA.Expr]
  }
  deriving (Eq, Ord)

instance Show TypeError where
  show (TypeError message exprs) = "Type Error: " ++ unlines (reverse message) ++ "\n\n" ++ formatExprs exprs

formatExprs :: [TA.Expr] -> String
formatExprs = concatMap (\t -> "\t" ++ formatExpr t ++ "\n")

formatExpr :: TA.Expr -> String
formatExpr e = show e ++ ": " ++ show (TA.getExprType e)
