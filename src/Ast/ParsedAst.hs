module Ast.ParsedAst where
import Lib.Types

data Module = Module String [ModulePath] [Function]
  deriving(Eq, Ord, Show)

data Function = Func Type Name [(Type, Name)] [Statement] Expr
  | Proc Name [(Type, Name)] [Statement]
  deriving(Eq, Ord, Show)

data Arg = Arg Type Name
  deriving (Eq, Ord, Show)
data Args = Args [Arg] | None
  deriving (Eq, Ord, Show)

data Statement
  = SExpr Expr
  | SDecl Name Type
  | SDeclAssign Name Type Expr
  | SBlock [Statement]
  | SWhile Expr Statement
  | SIf Expr Statement
  | ForEach Name Expr Statement
  | Kernel KExpr
  deriving (Eq, Ord, Show)

data Expr
 = BOp BinOp Expr Expr
 | Lambda [Name] Expr
 | EAssign Name Expr
 | Let [(Name, Expr)] Expr
 | EAssignArr Expr Expr Expr
 | UOp UnOp Expr
 | Lit Int IntSize SignType
 | FLit Double FloatSize
 | Var Name
 | ArrLit [Expr]
 | ListComp ListExpr
 | Ch Char
 | Call Name [Expr]
 | CCall Name [Expr]
 | Str String
  deriving (Eq, Ord, Show)

data KExpr
  = KBOp KBinOp KExpr KExpr
  | KName Name
   deriving (Eq, Ord, Show)

data ListExpr
  = LExpr Expr
  | LFor Expr Name ListExpr
  | LRange Expr (Maybe Expr) Expr
   deriving (Eq, Ord, Show)
