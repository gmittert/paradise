module Ast.ResolvedAst where
import Lib.Types

newtype ResolvedAst = ResolvedAst Prog
  deriving(Eq, Ord)
instance Show ResolvedAst where
  show (ResolvedAst p) = show p

newtype Prog = Prog [Function]
  deriving(Eq, Ord, Show)

data Function = Func Type Name [(Type, Name)] Statements
  deriving(Eq, Ord, Show)

data Statements
 = Statements' Statement
 | Statements Statements Statement
  deriving (Eq, Ord)

instance Show Statements where
  show (Statements' stmnt) = show stmnt
  show (Statements stmnts stmnt) = show stmnts ++ show stmnt

data Statement
  = SExpr Expr
  | SDecl Name Type
  | SDeclArr Name Type [Expr]
  | SDeclAssign Name Type Expr
  | SBlock Statements
  | SWhile Expr Statement
  | SIf Expr Statement
  | SReturn Expr
  deriving (Eq, Ord)
instance Show Statement where
  show (SExpr e) = show e ++ "; " ++ " \n"
  show (SDecl name tpe) = show tpe ++ " " ++ show name ++ "; " ++ "\n"
  show (SDeclAssign name tpe expr) = show tpe ++ " " ++ show name ++ " = " ++ show expr ++ "; " ++ "\n"
  show (SBlock b) = show b
  show (SWhile e stmnt) = "while (" ++ show e ++ ")\n" ++ show stmnt
  show (SIf e stmnt) = "if (" ++ show e ++ ")\n" ++ show stmnt
  show (SReturn e) = "return " ++ show e ++ "; " ++ "\n"

data Expr
 = BOp BinOp Expr Expr
 | EAssign Name Def Expr
 | EAssignArr Expr Expr Expr
 | UOp UnOp Expr
 | Lit Int
 | Var Name Def VarDir
 | FuncName Name Def
 | Ch Char
 | Call Name Def [Expr]
  deriving (Eq, Ord)

instance Show Expr where
  show (BOp op e1 e2) = show e1 ++ " " ++ show op ++ " " ++ show e2
  show (EAssign name _ expr) = show name ++ " = " ++ show expr
  show (EAssignArr e1 e2 e3) = show e1 ++ "[" ++ show e2 ++ "] = " ++ show e3
  show (UOp op e1) = show op ++ " " ++ show e1
  show (Lit i) = show i
  show (Var name _ _) = show name
  show (FuncName name _) = show name
  show (Ch char) = show char
  show (Call name _ exprs) = show name ++ "(" ++ show exprs ++ ")"
