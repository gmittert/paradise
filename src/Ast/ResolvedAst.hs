{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Ast.ResolvedAst where
import Types
import qualified Data.Map.Strict as M
import Control.Monad.State.Lazy
import Lib.SymbolTable


addVar :: Name -> Entry Statement -> SymbolTable Statement -> SymbolTable Statement
addVar name entry scope = scope {
  vars = M.insert name entry (vars scope)
}

addFunc :: Name -> Entry Statement -> SymbolTable Statement -> SymbolTable Statement
addFunc name entry scope = scope {
  funcs = M.insert name entry (funcs scope)
}

lookup :: Name -> Resolver (Entry Statement)
lookup name = Resolver . state $ \s ->
  case M.lookup name ((vars.symTab) s) of
    Just e -> (e,s)
    Nothing -> error $ "Could not find " ++ toString name
      ++ " in table " ++ show s

insert :: Name -> Type -> Statement -> Resolver ()
insert name typ decl =
  modify $ \s -> s{
    symTab = addVar name (Entry typ decl) (symTab s)
    }

newtype ResolveState
  = ResolveState {
    symTab :: SymbolTable Statement
  }
  deriving (Eq, Ord, Show)

emptyState :: ResolveState
emptyState = ResolveState emptyTable

newtype Resolver a = Resolver { resolve :: State ResolveState a }
  deriving (Functor, Applicative, Monad, MonadState ResolveState)

newtype ResolvedAst = ResolvedAst Prog
  deriving(Eq, Ord)
instance Show ResolvedAst where
  show (ResolvedAst p) = show p

data Prog
  = Prog Block (SymbolTable Statement)
  deriving(Eq, Ord)
instance Show Prog where
  show (Prog b _) = "Prog " ++ show b

data Block = Block Statements (SymbolTable Statement)
  deriving (Eq, Ord)
instance Show Block where
  show (Block stmnts _) = "{\n" ++ show stmnts ++ "}"

data Statements
 = Statements' Statement (SymbolTable Statement)
 | Statements Statements Statement (SymbolTable Statement)
  deriving (Eq, Ord)
instance Show Statements where
  show (Statements' stmnt _) = show stmnt
  show (Statements stmnts stmnt _) = show stmnts ++ show stmnt

data Statement
  = SExpr Expr (SymbolTable Statement)
  | SDecl Name Type (SymbolTable Statement)
  | SDeclAssign Name Type Expr (SymbolTable Statement)
  | SBlock Block (SymbolTable Statement)
  | SWhile Expr Statement (SymbolTable Statement)
  | SIf Expr Statement (SymbolTable Statement)
  | SReturn Expr (SymbolTable Statement)
  deriving (Eq, Ord)
instance Show Statement where
  show (SExpr e t) = show e ++ "; " ++ show t ++ " \n"
  show (SDecl name tpe t) = show tpe ++ " " ++ show name ++ "; " ++ show t ++ "\n"
  show (SDeclAssign name tpe expr t) = show tpe ++ " " ++ show name ++ " = " ++ show expr ++ "; " ++ show t ++ "\n"
  show (SBlock b _) = show b
  show (SWhile e stmnt _) = "while (" ++ show e ++ ")\n" ++ show stmnt
  show (SIf e stmnt _) = "if (" ++ show e ++ ")\n" ++ show stmnt
  show (SReturn e t) = "return " ++ show e ++ "; " ++ show t ++ "\n"

data Expr
 = BOp BinOp Expr Expr (SymbolTable Statement)
 | EAssign Name Expr (SymbolTable Statement)
 | UOp UnOp Expr (SymbolTable Statement)
 | Lit Int
 | Var Name (SymbolTable Statement)
 | Ch Char
  deriving (Eq, Ord)
instance Show Expr where
  show (BOp op e1 e2 _) = show e1 ++ " " ++ show op ++ " " ++ show e2
  show (EAssign name expr _) = show name ++ " = " ++ show expr
  show (UOp op e1 _) = show op ++ " " ++ show e1
  show (Lit i) = show i
  show (Var name _) = show name
  show (Ch char) = show char
