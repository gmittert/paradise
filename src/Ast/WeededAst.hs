{-# LANGUAGE DuplicateRecordFields #-}

module Ast.WeededAst where

import Lib.Types

data Module = Module
  -- The name of the module
  { mname :: ModulePath
  -- The other modules it imports
  , imports :: [ModulePath]
  -- The c functions it calls
  , cfuncs :: [CFunc]
  -- The functions it contains
  , funcs :: [Function]
  -- The type declarations
  , typs :: [TypeDec]
  -- The source loc of the module declaration
  , mposn :: Posn
  } deriving (Eq, Ord, Show)

data Function = Func
  { ret :: Type
  , fname :: Name
  , args :: [(Type, Name)]
  , body :: [Statement]
  , retVal :: Expr
  , fposn :: Posn
  } deriving (Eq, Ord, Show)

data Arg =
  Arg Type
      Name
      Posn
  deriving (Eq, Ord, Show)

data Args
  = Args [Arg]
  | None
  deriving (Eq, Ord, Show)

data Statement
  = SExpr { e1 :: Expr
          , posn :: Posn }
  | SDeclAssign { name :: Name
                , dtyp :: Maybe Type
                , e1 :: Expr
                , posn :: Posn }
  | SBlock [Statement]
           Posn
  | SWhile Expr
           Statement
           Posn
  | SIf Expr
        Statement
        Posn
  | ForEach { name :: Name
            , expr :: Expr
            , stm :: Statement
            , posn :: Posn }
  | Kernel KExpr
           Posn
  | Asm String
        [(String, Name)]
        [(String, Name)]
        (Maybe String)
        (Maybe String)
        Posn
  deriving (Eq, Ord, Show)

data Expr
  = BOp { bop :: BinOp
        , e1 :: Expr
        , e2 :: Expr
        , posn :: Posn }
  | UOp { uop :: UnOp
        , e1 :: Expr
        , posn :: Posn }
  | Lit { i :: Int
        , isz :: IntSize
        , sign :: SignType
        , posn :: Posn }
  | FLit { d :: Double
         , fsz :: FloatSize
         , posn :: Posn }
  | Var { n :: Name
        , posn :: Posn }
  | Ch { c :: Char
       , posn :: Posn }
  | Unit {posn :: Posn}
  | ArrLit { exprs :: [Expr]
           , posn :: Posn }
  | ListComp { lexprs :: ListExpr
             , posn :: Posn }
  | Call { fname :: Name
         , exprs :: [Expr]
         , posn :: Posn }
  | TypeConstr { cname :: Name
               , exprs :: [Expr]
               , posn :: Posn }
  | Case { e1 :: Expr
         , patexps :: [(Pattern, Expr)]
         , posn :: Posn }
  deriving (Eq, Ord, Show)

-- A pattern for pattern matching
data Pattern
  = PCh { c :: Char
        , posn :: Posn }
  | PLit { i :: Int
         , isz :: IntSize
         , st :: SignType
         , posn :: Posn }
  | PFLit { d :: Double
          , fsz :: FloatSize
          , posn :: Posn }
  | PVar { name :: Name
         , posn :: Posn }
  | PTypeConstr { name :: Name
                , pats :: [Pattern]
                , posn :: Posn }
  deriving (Eq, Ord, Show)

data KExpr
  = KBOp KBinOp
         KExpr
         KExpr
         Posn
  | KName { kname :: Name
          , posn :: Posn }
  deriving (Eq, Ord, Show)

data ListExpr
  = LFor Expr
         Name
         Expr
         Posn
  | LRange Expr
           Expr
           Expr
           Posn
  deriving (Eq, Ord, Show)
