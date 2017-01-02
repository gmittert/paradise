module Syntax where

import Types

data BinOp = Plus | Minus | Times | Div | Assign
  deriving (Eq, Ord, Show)

data Prog
  = Prog RetBlock CodegenState
  deriving(Eq, Ord, Show)

data RetBlock = Ret Args Statements Expr CodegenState
  deriving (Eq, Ord, Show)

data VoidBlock = Void Args Statements CodegenState
  deriving (Eq, Ord, Show)

data Arg = Arg Type Name
  deriving (Eq, Ord, Show)
data Args = Args [Arg] | None
  deriving (Eq, Ord, Show)

data Statements
 = Statements' Statement CodegenState
 | Statements Statements Statement CodegenState
  deriving (Eq, Ord, Show)

data Statement
  = SAssign Name Expr CodegenState
  | SExpr Expr CodegenState
  | SPrint Expr CodegenState
  | SDecl Name Type CodegenState
  | SDeclAssign Name Type Expr CodegenState
  | SBlock VoidBlock CodegenState
  deriving (Eq, Ord, Show)

data Expr
 = Op BinOp Name Expr CodegenState
 | Lit Int CodegenState
 | Str String CodegenState
 | Var Name CodegenState
 | Boolean Bool CodegenState
 | Ch Char CodegenState
 | EBlock RetBlock CodegenState
  deriving (Eq, Ord, Show)
