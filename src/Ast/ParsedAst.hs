module Ast.ParsedAst where

import Lib.Types

data Module =
  Module String
         [ModulePath]
         [CFunc]
         [Decl]
         Posn
  deriving (Eq, Ord, Show)

data Decl
  = FuncDecl Function
  | TypeDecl TypeDec
  deriving (Eq, Ord, Show)

data Function
  = Func Type
         Name
         [(Type, Name)]
         [Statement]
         Expr
         Posn
  | Proc Name
         [(Type, Name)]
         [Statement]
         Posn
  deriving (Eq, Ord, Show)

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
  = SExpr Expr
          Posn
  | SDecl { name :: Name
          , dtpe :: Type
          , posn :: Posn }
  | SDeclAssign Name
                Type
                Expr
                Posn
  | SBlock [Statement]
           Posn
  | SWhile Expr
           Statement
           Posn
  | SIf Expr
        Statement
        Posn
  | ForEach Name
            Expr
            Statement
            Posn
  | Kernel KExpr
           Posn
  | Asm String
        (Maybe [(String, Name)])
        (Maybe [(String, Name)])
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
  | Str { s :: String
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
  | LRange [Expr]
           Expr
           Posn
  deriving (Eq, Ord, Show)
