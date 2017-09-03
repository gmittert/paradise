module Ast.ParsedAst where
import Lib.Types

newtype Prog = Prog [Function]
  deriving(Eq, Ord, Show)

data Function = Func Type Name [(Type, Name)] Statements
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
  | SDeclArr Name Type [Expr]
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
 | Lit Int
 | Var Name
 | Ch Char
 | Call Name [Expr]
  deriving (Eq, Ord, Show)
