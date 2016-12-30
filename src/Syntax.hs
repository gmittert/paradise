module Syntax where

import Types

data BinOp = Plus | Minus | Times | Div | Assign
  deriving (Eq, Ord, Show)

data Type
  = Int
  | Bool
  | String Int
  | Char
  | Pointer Type
  deriving (Eq, Ord, Show)

toSize :: Type -> Int
toSize Int = 8
toSize (Pointer _) = 8
toSize Bool = 1
toSize (String a) = a + 16
toSize Char = 1

data SyntaxNode
  = NProg Prog
  | NStatements Statements
  | NStatement Statement
  | NExpr Expr

data Prog
  = Prog Statements Expr
  deriving (Eq, Ord, Show)

data Statements
 = Statements' Statement
 | Statements Statements Statement
  deriving (Eq, Ord, Show)

data Statement
  = SAssign Name Expr
  | SExpr Expr
  | SPrint Expr
  | SDecl Name Type
  | SDeclAssign Name Type Expr
  deriving (Eq, Ord, Show)

data Expr
 = Op BinOp Name Expr
 | Lit Int
 | Str String
 | Var Name
 | Boolean Bool
 | Ch Char
  deriving (Eq, Ord, Show)
