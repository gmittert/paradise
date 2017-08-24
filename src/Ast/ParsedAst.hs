module Ast.ParsedAst where
import Types

newtype Prog = Prog [Function]
  deriving(Eq, Ord, Show)

data Function = Func Type Name [Type] Statements
  deriving(Eq, Ord, Show)

data Arg = Arg Type Name
  deriving (Eq, Ord, Show)
data Args = Args [Arg] | None
  deriving (Eq, Ord, Show)

data Statements
 = Statements' Statement
 | Statements Statements Statement
  deriving (Eq, Ord, Show)

data Statement
  = SExpr Expr
  | SDecl Name Type
  | SDeclAssign Name Type Expr
  | SBlock Statements
  | SWhile Expr Statement
  | SIf Expr Statement
  | SReturn Expr
  deriving (Eq, Ord, Show)

data Expr
 = BOp BinOp Expr Expr
 | EAssign Name Expr
 | EAssignArr Expr Expr Expr
 | UOp UnOp Expr
 | EArr ExprList
 | Lit Int
 | Var Name
 | Ch Char
  deriving (Eq, Ord, Show)

data ExprList
 = Final Expr
 | List Expr ExprList
  deriving (Eq, Ord, Show)
