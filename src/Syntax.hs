module Syntax where

newtype Name = Name {toString :: String}
  deriving (Eq, Ord, Show)

data BinOp = Plus | Minus | Times | Div | Assign
  deriving (Eq, Ord, Show)

data Type
  = Int
  | Bool
  | String
  deriving (Eq, Ord, Show)

toSize :: Type -> Int
toSize Int = 8
toSize Bool = 1
toSize String = 8

data SyntaxNode
  = NProg Prog
  | NDecls Decls
  | NStatements Statements
  | NStatement Statement
  | NExpr Expr

data Prog
  = Prog Decls Statements Expr
  deriving (Eq, Ord, Show)

data Decls
  = Decls' Name Type
  | Decls Decls Name Type
  deriving (Eq, Ord, Show)

data Statements
 = Statements' Statement
 | Statements Statements Statement
  deriving (Eq, Ord, Show)

data Statement
  = SAssign Name Expr
  | SExpr Expr
  | SPrint Expr
  deriving (Eq, Ord, Show)

data Expr
 = Op BinOp Name Expr
 | Lit Int
 | Var Name
  deriving (Eq, Ord, Show)
