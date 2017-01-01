module Syntax where

import Types

data BinOp = Plus | Minus | Times | Div | Assign
  deriving (Eq, Ord, Show)

data Prog
  = Prog RetBlock SymbolTable
  deriving(Eq, Ord, Show)

data RetBlock = Ret Args Statements Expr SymbolTable
  deriving (Eq, Ord, Show)

data VoidBlock = Void Args Statements SymbolTable
  deriving (Eq, Ord, Show)

data Arg = Arg Type Name
  deriving (Eq, Ord, Show)
data Args = Args [Arg] | None
  deriving (Eq, Ord, Show)

data Statements
 = Statements' Statement SymbolTable
 | Statements Statements Statement SymbolTable
  deriving (Eq, Ord, Show)

data Statement
  = SAssign Name Expr SymbolTable
  | SExpr Expr SymbolTable
  | SPrint Expr SymbolTable
  | SDecl Name Type SymbolTable
  | SDeclAssign Name Type Expr SymbolTable
  | SBlock VoidBlock SymbolTable
  deriving (Eq, Ord, Show)

data Expr
 = Op BinOp Name Expr SymbolTable
 | Lit Int SymbolTable
 | Str String SymbolTable
 | Var Name SymbolTable
 | Boolean Bool SymbolTable
 | Ch Char SymbolTable
 | EBlock RetBlock SymbolTable
  deriving (Eq, Ord, Show)
