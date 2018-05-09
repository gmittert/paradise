module Ast.ResolvedAst where
import Lib.Types

newtype ResolvedAst = ResolvedAst Prog
  deriving(Eq, Ord)
instance Show ResolvedAst where
  show (ResolvedAst p) = show p

newtype Prog = Prog [Function]
  deriving(Eq, Ord, Show)

data Function
  = Func Type QualifiedName [(Type, Name)] [Statement] Expr
  deriving(Eq, Ord, Show)

data Statement
  = SExpr Expr
  | SDecl Name Type
  | SDeclAssign Name Type Expr
  | SBlock [Statement]
  | SWhile Expr Statement
  | SIf Expr Statement
  | ForEach Name Expr Statement
  | Kernel KExpr
  deriving (Eq, Ord)
instance Show Statement where
  show (SExpr e) = show e ++ "; " ++ " \n"
  show (SDecl name tpe) = show tpe ++ " " ++ show name ++ "; " ++ "\n"
  show (SDeclAssign name tpe expr) = show tpe ++ " " ++ show name ++ " = " ++ show expr ++ "; " ++ "\n"
  show (SBlock b) = show b
  show (SWhile e stmnt) = "while (" ++ show e ++ ")\n" ++ show stmnt
  show (SIf e stmnt) = "if (" ++ show e ++ ")\n" ++ show stmnt
  show (ForEach v e stmnt) = "for " ++ show v ++ " in " ++ show e ++ "\n" ++ show stmnt
  show (Kernel k) = "[| " ++ show k ++ " |]\n;"

data Expr
 = BOp BinOp Expr Expr
 | EAssign Name Def Expr
 | EAssignArr Expr Expr Expr
 | UOp UnOp Expr
 | Lit Int IntSize SignType
 | ArrLit [Expr]
 | FLit Double FloatSize
 | ListComp ListExpr
 | Var { newName :: Name, oldName :: Name, def::  Def, dir :: VarDir}
 | FuncName QualifiedName Def
 | Ch Char
 | Unit
 | Call QualifiedName Def [Expr]
 | CCall Name [Expr]
  deriving (Eq, Ord)

instance Show Expr where
  show (BOp op e1 e2) = show e1 ++ " " ++ show op ++ " " ++ show e2
  show (EAssign name _ expr) = show name ++ " = " ++ show expr
  show (EAssignArr e1 e2 e3) = show e1 ++ "[" ++ show e2 ++ "] = " ++ show e3
  show (UOp op e1) = show op ++ " " ++ show e1
  show (Lit i _ _) = show i
  show (FLit i _) = show i
  show (ArrLit exprs) = show exprs
  show (ListComp e) = show e
  show (Var name oldName _ _) = show name ++ "(" ++ show oldName ++  ")"
  show (FuncName name _) = show name
  show (Ch char) = show char
  show Unit = "()"
  show (Call name _ exprs) = show name ++ "(" ++ show exprs ++ ")"

data KExpr
  = KBOp KBinOp KExpr KExpr
  | KName Name Def
  deriving (Eq, Ord, Show)

data ListExpr
  = LExpr Expr
  | LFor Expr Name ListExpr
  | LRange Expr Expr Expr
   deriving (Eq, Ord, Show)
