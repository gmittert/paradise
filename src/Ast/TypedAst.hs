{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Ast.TypedAst where
import Types
import Lib.SymbolTable
import Control.Monad.State.Lazy

data Entry = Entry Type Statement
  deriving (Eq, Ord, Show)

newtype TypeState
  = TypeState {
    symTab :: SymbolTable Statement
  }
  deriving (Eq, Ord, Show)

emptyState :: TypeState
emptyState = TypeState emptyTable

newtype Typer a = Typer { runTyper :: State TypeState a }
  deriving (Functor, Applicative, Monad, MonadState TypeState)


newtype TypedAst = TypedAst Prog
  deriving(Eq, Ord)
instance Show TypedAst where
  show (TypedAst p) = show p

data Prog
  = Prog Block (SymbolTable Statement) Type
  deriving(Eq, Ord)
instance Show Prog where
  show (Prog b _ _) = "Prog " ++ show b

data Block = Block Statements (SymbolTable Statement) Type
  deriving (Eq, Ord)
instance Show Block where
  show (Block stmnts _ _) = "{\n" ++ show stmnts ++ "}"

data Statements
 = Statements' Statement (SymbolTable Statement) Type
 | Statements Statements Statement (SymbolTable Statement) Type
  deriving (Eq, Ord)
instance Show Statements where
  show (Statements' stmnt _ _) = show stmnt
  show (Statements stmnts stmnt _ _) = show stmnts ++ show stmnt

data Statement
  = SExpr Expr (SymbolTable Statement) Type
  | SDecl Name Type (SymbolTable Statement) Type
  | SDeclAssign Name Type Expr (SymbolTable Statement) Type
  | SBlock Block (SymbolTable Statement) Type
  | SWhile Expr Statement (SymbolTable Statement) Type
  | SIf Expr Statement (SymbolTable Statement) Type
  | SReturn Expr (SymbolTable Statement) Type
  deriving (Eq, Ord)
instance Show Statement where
  show (SExpr e _ _) = show e ++ ";\n"
  show (SDecl name tpe _ _) = show tpe ++ " " ++ show name ++ ";\n"
  show (SDeclAssign name tpe expr _ _) = show tpe ++ " " ++ show name ++ " = " ++ show expr ++ ";\n"
  show (SBlock b _ _) = show b
  show (SWhile e stmnt _ _) = "while (" ++ show e ++ ")\n" ++ show stmnt
  show (SIf e stmnt _ _) = "if (" ++ show e ++ ")\n" ++ show stmnt
  show (SReturn e _ _) = "return " ++ show e ++ ";\n"

data Expr
 = BOp BinOp Expr Expr (SymbolTable Statement) Type
 | EAssign Name Expr (SymbolTable Statement) Type
 | UOp UnOp Expr (SymbolTable Statement) Type
 | Lit Int
 | Var Name (SymbolTable Statement) Type
 | Ch Char
  deriving (Eq, Ord)
instance Show Expr where
  show (BOp op e1 e2 _ _) = show e1 ++ " " ++ show op ++ " " ++ show e2
  show (EAssign name expr _ _) = show name ++ " = " ++ show expr
  show (UOp op e1 _ _) = show op ++ " " ++ show e1
  show (Lit i) = show i
  show (Var name _ _) = show name
  show (Ch char) = show char

{-
  Extract the type attached to a statement
-}
getStmntType :: Statement -> Type
getStmntType (SExpr _ _ tpe) = tpe
getStmntType (SDecl _ _ _ tpe) = tpe
getStmntType (SDeclAssign _ _ _ _ tpe) = tpe
getStmntType (SBlock _  _ tpe) = tpe
getStmntType (SWhile _ _  _ tpe) = tpe
getStmntType (SIf _ _  _ tpe) = tpe
getStmntType (SReturn _  _ tpe) = tpe

{-
  Extract the table attached to a statement
-}
getStmntTable :: Statement -> SymbolTable Statement
getStmntTable (SExpr _ table _) = table
getStmntTable (SDecl _ _ table _) = table
getStmntTable (SDeclAssign _ _ _ table _) = table
getStmntTable (SBlock _  table _) = table
getStmntTable (SWhile _ _  table _) = table
getStmntTable (SIf _ _  table _) = table
getStmntTable (SReturn _  table _) = table

getExprType :: Expr -> Type
getExprType (BOp _ _ _ _ tpe) = tpe
getExprType (UOp _ _ _ tpe) = tpe
getExprType (EAssign _ _ _ tpe) = tpe
getExprType (Lit _)  = Int
getExprType (Var _ _ tpe)  = tpe
getExprType (Ch _)  = Char

getExprTable :: Expr -> SymbolTable Statement
getExprTable (BOp _ _ _ table _) = table
getExprTable (UOp _ _ table _) = table
getExprTable (EAssign _ _ table _) = table
getExprTable (Lit _)  = emptyTable
getExprTable (Var _ table _)  = table
getExprTable (Ch _)  = emptyTable
