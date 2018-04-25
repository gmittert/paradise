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
  , body:: [Statement]
  , retVal :: Expr
  }
  | Proc {
      fname :: Name
      , args :: [(Type, Name)]
      , body:: [Statement]
  }
  | CFunc {
  ret ::Type
  , fname :: Name
  , args :: [(Type, Name)]
  , cbody :: String
  }
  deriving(Eq, Ord, Show)

data Arg = Arg Type Name
  deriving (Eq, Ord, Show)
data Args = Args [Arg] | None
  deriving (Eq, Ord, Show)

data Statement
  = SExpr Expr
  | SDecl Name Type
  | SDeclArr Name Type [Expr]
  | SDeclAssign Name Type Expr
  | SBlock [Statement]
  | SWhile Expr Statement
  | SIf Expr Statement
  | ForEach Name Expr Statement
  | Kernel KExpr
  deriving (Eq, Ord, Show)

data Expr
 = BOp BinOp Expr Expr
 | EAssign Name Expr
 | EAssignArr Expr Expr Expr
 | UOp UnOp Expr
 | Lit Int IntSize SignType
 | FLit Double FloatSize
 | Var Name
 | Ch Char
 | Call Name [Expr]
  deriving (Eq, Ord, Show)

data KExpr
  = KBOp KBinOp KExpr KExpr
  | KName Name
  deriving (Eq, Ord, Show)
