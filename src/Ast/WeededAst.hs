{-# LANGUAGE DuplicateRecordFields #-}
module Ast.WeededAst where
import Lib.Types

data Module = Module {
  -- The name of the module
  mname :: ModulePath,
  -- The other modules it imports
  imports::[ModulePath],
  -- The functions it contains
  funcs::[Function]}
  deriving(Eq, Ord, Show)

data Function = Func {
  ret ::Type
  , fname :: Name
  , args :: [(Type, Name)]
  , body:: Statements
  , retVal :: Expr
  }
  | Proc {
      fname :: Name
      , args :: [(Type, Name)]
      , body:: Statements
  }
  | AsmFunc {
  ret ::Type
  , fname :: Name
  , args :: [(Type, Name)]
  , asmbody :: String
  }
  deriving(Eq, Ord, Show)

data Arg = Arg Type Name
  deriving (Eq, Ord, Show)
data Args = Args [Arg] | None
  deriving (Eq, Ord, Show)

data Statements
 = Statements' Statement
 | Statements Statements Statement
  deriving (Eq, Ord, Show)

data Statement
  = SExpr Expr
  | SDecl Name Type
  | SDeclArr Name Type [Expr]
  | SDeclAssign Name Type Expr
  | SBlock Statements
  | SWhile Expr Statement
  | SIf Expr Statement
  deriving (Eq, Ord, Show)

data Expr
 = BOp BinOp Expr Expr
 | EAssign Name Expr
 | EAssignArr Expr Expr Expr
 | UOp UnOp Expr
 | Lit Int IntSize SignType
 | Var Name
 | Ch Char
 | Call Name [Expr]
  deriving (Eq, Ord, Show)
