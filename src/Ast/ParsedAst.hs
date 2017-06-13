module Ast.ParsedAst where
import Types

data ParsedAst = ParsedAst Prog

data Prog
  = Prog Block
  deriving(Eq, Ord, Show)

data Block = Block Statements
  deriving (Eq, Ord, Show)

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
  | SBlock Block
  | SWhile Expr Statement
  | SIf Expr Statement
  | SReturn Expr
  deriving (Eq, Ord, Show)

data Expr
 = BOp BinOp Expr Expr
 | EAssign Name Expr
 | UOp UnOp Expr
 | Lit Int
 | Var Name
 | Ch Char
  deriving (Eq, Ord, Show)
