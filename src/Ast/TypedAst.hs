{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Ast.TypedAst where
import Lib.Types
import Lib.SymbolTable
import Control.Monad.State.Lazy

newtype TypeState
  = TypeState {
    symTab :: SymbolTable
  }
  deriving (Eq, Ord, Show)

emptyState :: TypeState
emptyState = TypeState emptyTable

newtype Typer a = Typer { runTyper :: State TypeState a }
  deriving (Functor, Applicative, Monad, MonadState TypeState)

newtype Prog = Prog [Function]
  deriving(Eq, Ord)
instance Show Prog where
  show (Prog f) = join $ show <$> f

data Function = Func Type Name [(Type, Name)] Statements
  deriving(Eq, Ord)
instance Show Function where
  show (Func tpe name tps stmnt) = show tpe ++ " " ++ toString name ++ show tps ++ show stmnt

data Statements
 = Statements' Statement Type
 | Statements Statements Statement Type
  deriving (Eq, Ord)
instance Show Statements where
  show (Statements' stmnt _) = show stmnt
  show (Statements stmnts stmnt _) = show stmnts ++ show stmnt

data Statement
  = SExpr Expr Type
  | SDecl Name Type Type
  | SDeclArr Name Type [Expr] Type
  | SDeclAssign Name Type Expr Type
  | SBlock Statements Type
  | SWhile Expr Statement Type
  | SIf Expr Statement Type
  | SReturn Expr Type
  deriving (Eq, Ord)
instance Show Statement where
  show (SExpr e _) = show e ++ ";\n"
  show (SBlock s _) = "{\n" ++ show s ++ "\n}"
  show (SDecl name tpe _) = show tpe ++ " " ++ show name ++ ";\n"
  show (SDeclAssign name tpe expr _) = show tpe ++ " " ++ show name ++ " = " ++ show expr ++ ";\n"
  show (SWhile e stmnt _) = "while (" ++ show e ++ ")\n" ++ show stmnt
  show (SIf e stmnt _) = "if (" ++ show e ++ ")\n" ++ show stmnt
  show (SReturn e _) = "return " ++ show e ++ ";\n"

data Expr
 = BOp BinOp Expr Expr Type
 | EAssign Name Expr Type
 | EAssignArr Expr Expr Expr Type
 | UOp UnOp Expr Type
 | Lit Int
 | Var Name Type VarDir
 | FuncName Name Type
 | Ch Char
 | Call Name Def [Expr] Type
  deriving (Eq, Ord)
instance Show Expr where
  show (BOp op e1 e2 _) = show e1 ++ " " ++ show op ++ " " ++ show e2
  show (EAssign name expr _) = show name ++ " = " ++ show expr
  show (EAssignArr e1 e2 e3 _) = show e1 ++ "[" ++ show e2 ++ "] = " ++ show e3
  show (UOp op e1 _) = show op ++ " " ++ show e1
  show (Lit i) = show i
  show (Var name _ _) = show name
  show (FuncName name _) = show name
  show (Ch char) = show char
  show (Call name _ exprs _) = show name ++ "(" ++ show exprs ++ ")"

{-
  Extract the type attached to a statement
-}
getStmntType :: Statement -> Type
getStmntType (SExpr _ tpe) = tpe
getStmntType (SDecl _ _ tpe) = tpe
getStmntType (SDeclAssign _ _ _ tpe) = tpe
getStmntType (SBlock _ tpe) = tpe
getStmntType (SWhile _ _ tpe) = tpe
getStmntType (SIf _ _ tpe) = tpe
getStmntType (SReturn _ tpe) = tpe

{-
  Extract the table attached to a statement
-}
getExprType :: Expr -> Type
getExprType (BOp _ _ _ tpe) = tpe
getExprType (UOp _ _ tpe) = tpe
getExprType (EAssign _ _ tpe) = tpe
getExprType (Lit _)  = Int
getExprType (Var _ tpe _)  = tpe
getExprType (FuncName _ tpe)  = tpe
getExprType (Ch _)  = Char
getExprType (EAssignArr _ _ _ tpe) = tpe
getExprType (Call _ _ _ tpe) = tpe
