module Syntax where

import Types

data BinOp = Plus | Minus | Times | Div | Assign | Lt | Lte | Access
  deriving (Eq, Ord, Show)

data UnOp = Deref
  deriving (Eq, Ord, Show)

data Prog
  = Prog Block CodegenState
  deriving(Eq, Ord, Show)

data Block = Block Statements CodegenState
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
  = SExpr Expr CodegenState
  | SPrint Expr CodegenState
  | SDecl Name Type CodegenState
  | SDeclArr Name Type Int CodegenState
  | SDeclAssign Name Type Expr CodegenState
  | SBlock Block CodegenState
  | SWhile Expr Statement CodegenState
  | SIf Expr Statement CodegenState
  | SReturn Expr CodegenState
  deriving (Eq, Ord, Show)

data Expr
 = BOp BinOp Name Expr CodegenState
 | EAssign Expr Expr CodegenState
 | UOp UnOp Expr CodegenState
 | Lit Int CodegenState
 | Str String CodegenState
 | Var Name CodegenState
 | Boolean Bool CodegenState
 | Ch Char CodegenState
  deriving (Eq, Ord, Show)
