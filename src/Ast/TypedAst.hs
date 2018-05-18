{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Ast.TypedAst where
import Lib.Types
import qualified Lib.SymbolTable as ST
import Control.Monad.State.Lazy

data TypeState
  = TypeState {
    symTab :: ST.SymbolTable
    , context :: [Expr]
    , message :: [String]
  }
  deriving (Eq, Ord, Show)

emptyState :: TypeState
emptyState = TypeState mempty [] []

newtype Typer a = Typer { runTyper :: State TypeState a }
  deriving (Functor, Applicative, Monad, MonadState TypeState)

data Module = Module
  -- The name of the module
  { mname :: ModulePath
  -- The other modules it imports
  , imports :: [ModulePath]
  -- The c functions it calls
  , cfuncs :: [CFunc]
  -- The functions it contains
  , funcs :: [Function]
  -- The symbol table for the module
  , symtab :: ST.SymbolTable
  } deriving (Eq, Ord, Show)

data Function
  = Func {
    retType :: Type
    , name :: QualifiedName
    , args :: [(Type, Name)]
    , body :: [Statement]
    , ret :: Expr
    }
  deriving(Eq, Ord)
instance Show Function where
  show (Func tpe name tps stmnt expr) = concat [show tpe, " ", show name, show tps, show stmnt, show expr]

data Statement
  = SExpr Expr Type
  | SDecl Name Type Type
  | SDeclAssign Name Type Expr Type
  | SBlock [Statement] Type
  | SWhile Expr Statement Type
  | SIf Expr Statement Type
  | ForEach Name Expr Statement Type
  | Kernel KExpr Type
  deriving (Eq, Ord)
instance Show Statement where
  show (SExpr e _) = concat [show e, ";\n"]
  show (SBlock s _) = concat ["{\n", show s, "\n}"]
  show (SDecl name tpe _) = show tpe ++ " " ++ show name ++ ";\n"
  show (SDeclAssign name tpe expr _) = show tpe ++ " " ++ show name ++ " = " ++ show expr ++ ";\n"
  show (SWhile e stmnt _) = "while (" ++ show e ++ ")\n" ++ show stmnt
  show (SIf e stmnt _) = "if (" ++ show e ++ ")\n" ++ show stmnt
  show (ForEach name e stmnt _) = "for " ++ show name ++ " in " ++ show e ++ "\n" ++ show stmnt
  show (Kernel k _) = "[| " ++ show k ++ " |]\n;"

data Expr
 = BOp BinOp Expr Expr Type
 | UOp UnOp Expr Type
 | Lit Int IntSize SignType
 | FLit Double FloatSize
 | Unit
 | Var {name :: Name, oldName:: Name, tpe:: Type, dir:: VarDir }
 | ArrLit [Expr] Type
 | ListComp ListExpr Type
 | FuncName QualifiedName Type
 | Ch Char
 | Call QualifiedName Def [Expr] Type
 | CCall Name [Expr]
  deriving (Eq, Ord)
instance Show Expr where
  show (BOp op e1 e2 _) = show e1 ++ " " ++ show op ++ " " ++ show e2
  show (UOp op e1 _) = show op ++ " " ++ show e1
  show (Lit i _ _) = show i
  show (ArrLit exprs _) = show exprs
  show (FLit i _) = show i
  show (Var name oldName _ _) = show name ++ "(" ++ show oldName ++ ")"
  show (FuncName name _) = show name
  show (Ch char) = show char
  show Unit = "()"
  show (Call name _ exprs _) = show name ++ "(" ++ show exprs ++ ")"
  show (CCall name exprs) = show name ++ "(" ++ show exprs ++ ")"
  show (ListComp l _) = show l

data KExpr
  = KBOp KBinOp KExpr KExpr Type
  | KName Name Def Type
  deriving (Eq, Ord, Show)

data ListExpr
  = LFor Expr Name Expr Type
  | LRange Expr Expr Expr Type
   deriving (Eq, Ord, Show)

{-
  Extract the type attached to a statement
-}
getStmntType :: Statement -> Type
getStmntType (SExpr _ tpe) = tpe
getStmntType (SDecl _ _ tpe) = tpe
getStmntType (SDeclAssign _ _ _ tpe) = tpe
getStmntType (SBlock _ tpe) = tpe
getStmntType (SWhile _ _ tpe) = tpe
getStmntType (SIf _ _ tpe) = tpe
getStmntType (ForEach _ _ _ tpe) = tpe
getStmntType (Kernel _ tpe) = tpe

{-
  Extract the type attached to an expr
-}
getExprType :: Expr -> Type
getExprType (BOp _ _ _ tpe) = tpe
getExprType (UOp _ _ tpe) = tpe
getExprType (Lit _ sz s)  = Int sz s
getExprType Unit = Void
getExprType (FLit _ sz)  = Float sz
getExprType (ArrLit _ t)  = t
getExprType (ListComp _ t)  = t
getExprType (Var _ _ tpe _)  = tpe
getExprType (FuncName _ tpe)  = tpe
getExprType (Ch _)  = Char
getExprType (Call _ _ _ tpe) = tpe
getExprType (CCall _ _) = error "No type on CCall"

{-
  Extract the type of a KExpr
-}
getKExprType :: KExpr -> Type
getKExprType (KBOp _ _ _ t) = t
getKExprType (KName _ _ t) = t

getListExprType :: ListExpr -> Type
getListExprType (LFor _ _ _ t) = t
getListExprType (LRange _ _ _ t) = t
