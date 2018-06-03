module Ast.ResolvedAst where

import Data.Maybe
import Lib.Format
import qualified Lib.SymbolTable as ST
import Lib.Types

data Module = Module
  -- The name of the module
  { mname :: ModulePath
  -- The other modules it imports
  , imports :: [ModulePath]
  -- The functions
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

data Function =
  Func Type
       QualifiedName
       [(Type, Name)]
       [Statement]
       Expr
  deriving (Eq, Ord, Show)

data Statement
  = SExpr Expr
  | SDecl Name
          Type
  | SDeclAssign Name
                Type
                Expr
  | SBlock [Statement]
  | SWhile Expr
           Statement
  | SIf Expr
        Statement
  | ForEach Name
            Expr
            Statement
  | Kernel KExpr
  | Asm String
        [(String, Expr)]
        [(String, Expr)]
        (Maybe String)
        (Maybe String)
        Posn
  deriving (Eq, Ord)

instance Show Statement where
  show (SExpr e) = show e ++ "; " ++ " \n"
  show (SDecl name tpe) = show tpe ++ " " ++ show name ++ "; " ++ "\n"
  show (SDeclAssign name tpe expr) =
    show tpe ++ " " ++ show name ++ " = " ++ show expr ++ "; " ++ "\n"
  show (SBlock b) = show b
  show (SWhile e stmnt) = "while (" ++ show e ++ ")\n" ++ show stmnt
  show (SIf e stmnt) = "if (" ++ show e ++ ")\n" ++ show stmnt
  show (ForEach v e stmnt) =
    "for " ++ show v ++ " in " ++ show e ++ "\n" ++ show stmnt
  show (Kernel k) = "[| " ++ show k ++ " |]\n;"
  show (Asm e o i c _ _) =
    concat
      [ "asm ("
      , e
      , ":"
      , (commaListS . map (\(s, n) -> s ++ "(" ++ show n ++ ")")) o
      , (commaListS . map (\(s, n) -> s ++ "(" ++ show n ++ ")")) i
      , fromMaybe "" c
      , fromMaybe "" c
      ]

data Expr
  = BOp BinOp
        Expr
        Expr
  | UOp UnOp
        Expr
  | Lit Int
        IntSize
        SignType
  | ArrLit [Expr]
  | FLit Double
         FloatSize
  | ListComp ListExpr
  | Var { newName :: Name
        , oldName :: Name
        , def :: Def
        , dir :: VarDir }
  | FuncName QualifiedName
             Def
  | Ch Char
  | Unit
  | Call QualifiedName
         Def
         [Expr]
  | CCall Name
          CFunc
          [Expr]
  | TypeConstr { cname :: Name
               , typDec :: TypeDec
               , exprs :: [Expr]
               , posn :: Posn }
  | Case { e1 :: Expr
         , patexps :: [(Pattern, Expr)]
         , posn :: Posn }
  deriving (Eq, Ord)

instance Show Expr where
  show (BOp op e1 e2) = show e1 ++ " " ++ show op ++ " " ++ show e2
  show (UOp op e1) = show op ++ " " ++ show e1
  show (Lit i _ _) = show i
  show (FLit i _) = show i
  show (ArrLit exprs) = show exprs
  show (ListComp e) = show e
  show (Var name oldName _ _) = show name ++ "(" ++ show oldName ++ ")"
  show (FuncName name _) = show name
  show (Ch char) = show char
  show Unit = "()"
  show (Call name _ exprs) = show name ++ "(" ++ show exprs ++ ")"
  show (CCall name _ exprs) = show name ++ "(" ++ show exprs ++ ")"
  show (TypeConstr name _ exprs _) = show name ++ "(" ++ show exprs ++ ")"
  show (Case e1 patexps _) = "case " ++ show e1 ++ " of " ++ show patexps

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
                , typDec :: TypeDec
                , pats :: [Pattern]
                , posn :: Posn }
  deriving (Eq, Ord, Show)

data KExpr
  = KBOp KBinOp
         KExpr
         KExpr
  | KName Name
          Def
  deriving (Eq, Ord, Show)

data ListExpr
  = LFor Expr
         Name
         Expr
  | LRange Expr
           Expr
           Expr
  deriving (Eq, Ord, Show)
