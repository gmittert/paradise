module Ast.TypedAst where
import Types
import Lib.SymbolTable

data Entry = Entry Type Statement
  deriving (Eq, Ord, Show)

data TypedAst = TypedAst Prog

data Prog
  = Prog Block (SymbolTable Statement) Type
  deriving(Eq, Ord, Show)

data Block = Block Statements (SymbolTable Statement) Type
  deriving (Eq, Ord, Show)

data Statements
 = Statements' Statement (SymbolTable Statement) Type
 | Statements Statements Statement (SymbolTable Statement) Type
  deriving (Eq, Ord, Show)

data Statement
  = SExpr Expr (SymbolTable Statement) Type
  | SDecl Name Type (SymbolTable Statement) Type
  | SDeclAssign Name Type Expr (SymbolTable Statement) Type
  | SBlock Block (SymbolTable Statement) Type
  | SWhile Expr Statement (SymbolTable Statement) Type
  | SIf Expr Statement (SymbolTable Statement) Type
  | SReturn Expr (SymbolTable Statement) Type
  deriving (Eq, Ord, Show)

data Expr
 = BOp BinOp Expr Expr (SymbolTable Statement) Type
 | EAssign Name Expr (SymbolTable Statement) Type
 | UOp UnOp Expr (SymbolTable Statement) Type
 | Lit Int
 | Var Name (SymbolTable Statement) Type
 | Ch Char
  deriving (Eq, Ord, Show)

getStmntType :: Statement -> Type
getStmntType (SExpr _ _ tpe) = tpe
getStmntType (SDecl _ _ _ tpe) = tpe
getStmntType (SDeclAssign _ _ _ _ tpe) = tpe
getStmntType (SBlock _  _ tpe) = tpe
getStmntType (SWhile _ _  _ tpe) = tpe
getStmntType (SIf _ _  _ tpe) = tpe
getStmntType (SReturn _  _ tpe) = tpe

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
