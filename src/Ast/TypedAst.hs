{-# LANGUAGE DuplicateRecordFields #-}

module Ast.TypedAst where

import Data.Maybe
import Lib.Format
import qualified Lib.SymbolTable as ST
import Lib.Types

data Module = Module
  -- The name of the module
  { mname :: ModulePath
  -- The other modules it imports
  , imports :: [ModulePath]
  , importFuncs :: [(QualifiedName, Def)]
  -- The c functions it calls
  , cfuncs :: [CFunc]
  -- The functions it contains
  , funcs :: [Function]
  -- The type declarations
  , typs :: [Lib.Types.TypeDec]
  -- The symbol table for the module
  , symtab :: ST.SymbolTable
  } deriving (Eq, Ord, Show)

data Function = Func
  { retType :: Type
  , name :: QualifiedName
  , args :: [(Type, Name)]
  , body :: [Statement]
  , ret :: Expr
  } deriving (Eq, Ord)

instance Show Function where
  show (Func tpe name tps stmnt expr) =
    concat [show tpe, " ", show name, show tps, show stmnt, show expr]

data Statement
  = SExpr Expr
          Type
  | SDeclAssign Name
                Type
                Expr
                Type
  | SBlock [Statement]
           Type
  | SWhile Expr
           Statement
           Type
  | SIf Expr
        Statement
        Type
  | ForEach Name
            Expr
            Statement
            Type
  | Kernel KExpr
           Type
  | Asm String
        [(String, Expr)]
        [(String, Expr)]
        (Maybe String)
        (Maybe String)
        Posn
        Type
  deriving (Eq, Ord)

instance Show Statement where
  show (SExpr e _) = show e ++ ";\n"
  show (SBlock s _) = concat ["{\n", show s, "\n}"]
  show (SDeclAssign name tpe expr _) =
    show tpe ++ " " ++ show name ++ " = " ++ show expr ++ ";\n"
  show (SWhile e stmnt _) = "while (" ++ show e ++ ")\n" ++ show stmnt
  show (SIf e stmnt _) = "if (" ++ show e ++ ")\n" ++ show stmnt
  show (ForEach name e stmnt _) =
    "for " ++ show name ++ " in " ++ show e ++ "\n" ++ show stmnt
  show (Kernel k _) = "[| " ++ show k ++ " |]\n;"
  show (Asm e o i c opt _ _) =
    concat
      [ "asm ("
      , e
      , ":"
      , (commaListS . map (\(s, n) -> s ++ "(" ++ show n ++ ")")) o
      , (commaListS . map (\(s, n) -> s ++ "(" ++ show n ++ ")")) i
      , fromMaybe "" c
      , fromMaybe "" opt
      ]

data Expr
  = BOp { bop :: BinOp
        , e1 :: Expr
        , e2 :: Expr
        , tpe :: Type }
  | UOp { uop :: UnOp
        , e1 :: Expr
        , tpe :: Type }
  | Lit { i :: Int
        , isz :: IntSize
        , st :: SignType
        , tpe :: Type }
  | FLit { d :: Double
         , fsz :: FloatSize
         , tpe :: Type }
  | Unit { tpe :: Type }
  | Var { name :: Name
        , oldName :: Name
        , def :: Def
        , tpe :: Type
        , dir :: VarDir }
  | ArrLit { exprs :: [Expr]
           , tpe :: Type }
  | ListComp { lexprs :: ListExpr
             , tpe :: Type }
  | FuncName { qname :: QualifiedName
             , tpe :: Type }
  | Ch { c :: Char
       , tpe :: Type }
  | Call { qname :: QualifiedName
         , def :: Def
         , args :: [Expr]
         , tpe :: Type }
  | CCall { name :: Name
          , cdef :: CFunc
          , args :: [Expr]
          , tpe :: Type }
  | TypeConstr { cname :: Name
               , targs :: [Type]
               , typDec :: TypeDec
               , exprs :: [Expr]
               , posn :: Posn
               , tpe :: Type }
  | Case { e1 :: Expr
         , patexps :: [(Pattern, Expr)]
         , posn :: Posn
         , tpe :: Type }
  deriving (Eq, Ord)

instance Show Expr where
  show (BOp op e1 e2 _) = show e1 ++ " " ++ show op ++ " " ++ show e2
  show (UOp op e1 _) = show op ++ " " ++ show e1
  show (Lit i _ _ _) = show i
  show (ArrLit exprs _) = show exprs
  show (FLit i _ _) = show i
  show (Var name oldName _ _ _) = show name ++ "(" ++ show oldName ++ ")"
  show (FuncName name _) = show name
  show (Ch char _) = show char
  show (Unit _) = "()"
  show (Call name _ exprs _) = show name ++ "(" ++ show exprs ++ ")"
  show (CCall name _ exprs _) = show name ++ "(" ++ show exprs ++ ")"
  show (ListComp l _) = show l
  show (TypeConstr name _ _ exprs _ _) = show name ++ "(" ++ show exprs ++ ")"
  show (Case e1 patexps _ _) = "case " ++ show e1 ++ " of " ++ show patexps

-- A pattern for pattern matching
data Pattern
  = PCh { c :: Char
        , posn :: Posn
        , tpe :: Type }
  | PLit { i :: Int
         , isz :: IntSize
         , st :: SignType
         , posn :: Posn
         , tpe :: Type }
  | PFLit { d :: Double
          , fsz :: FloatSize
          , posn :: Posn
          , tpe :: Type }
  | PVar { name :: Name
         , posn :: Posn
         , tpe :: Type }
  | PTypeConstr { name :: Name
                , typDec :: TypeDec
                , pats :: [Pattern]
                , posn :: Posn
                , tpe :: Type }
  deriving (Eq, Ord, Show)

data KExpr
  = KBOp KBinOp
         KExpr
         KExpr
         Type
  | KName Name
          Def
          Type
  deriving (Eq, Ord, Show)

data ListExpr
  = LFor Expr
         Name
         Expr
         Type
  | LRange Expr
           Expr
           Expr
           Type
  deriving (Eq, Ord, Show)

{-
  Extract the type attached to a statement
-}
getStmntType :: Statement -> Type
getStmntType (SExpr _ tpe) = tpe
getStmntType (SDeclAssign _ _ _ tpe) = tpe
getStmntType (SBlock _ tpe) = tpe
getStmntType (SWhile _ _ tpe) = tpe
getStmntType (SIf _ _ tpe) = tpe
getStmntType (ForEach _ _ _ tpe) = tpe
getStmntType (Kernel _ tpe) = tpe
getStmntType (Asm _ _ _ _ _ _ tpe) = tpe

{-
  Extract the type of a KExpr
-}
getKExprType :: KExpr -> Type
getKExprType (KBOp _ _ _ t) = t
getKExprType (KName _ _ t) = t

getListExprType :: ListExpr -> Type
getListExprType (LFor _ _ _ t) = t
getListExprType (LRange _ _ _ t) = t
