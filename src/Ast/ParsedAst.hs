module Ast.ParsedAst where
import Lib.Types

data Module = Module String [ModulePath] [Function]
  deriving(Eq, Ord, Show)

data Function = Func Type Name [(Type, Name)] Statements
              | AsmFunc Type Name [(Type, Name)] String
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
 | Lambda [Name] Expr
 | EAssign Name Expr
 | Let [(Name, Expr)] Expr
 | ERefAssign Name Expr
 | EAssignArr Expr Expr Expr
 | UOp UnOp Expr
 | Lit Int IntSize SignType
 | Var Name
 | Ch Char
 | Call Name [Expr]
  deriving (Eq, Ord, Show)
