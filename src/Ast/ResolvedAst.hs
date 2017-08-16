{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Ast.ResolvedAst where
import Types
import qualified Data.Map.Strict as M
import Control.Monad.State.Lazy
import Lib.SymbolTable


addVar :: Name -> (Entry Statement) -> (SymbolTable Statement) -> (SymbolTable Statement)
addVar name entry scope = scope {
  vars = M.insert name entry (vars scope)
}

addFunc :: Name -> (Entry Statement) -> (SymbolTable Statement) -> (SymbolTable Statement)
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

data ResolveState
  = ResolveState {
    symTab :: (SymbolTable Statement)
  }
  deriving (Eq, Ord, Show)

emptyState :: ResolveState
emptyState = ResolveState emptyTable

newtype Resolver a = Resolver { resolve :: State ResolveState a }
  deriving (Functor, Applicative, Monad, MonadState ResolveState)

data ResolvedAst = ResolvedAst Prog

data Prog
  = Prog Block (SymbolTable Statement)
  deriving(Eq, Ord, Show)

data Block = Block Statements (SymbolTable Statement)
  deriving (Eq, Ord, Show)

data Statements
 = Statements' Statement (SymbolTable Statement)
 | Statements Statements Statement (SymbolTable Statement)
  deriving (Eq, Ord, Show)

data Statement
  = SExpr Expr (SymbolTable Statement)
  | SDecl Name Type (SymbolTable Statement)
  | SDeclAssign Name Type Expr (SymbolTable Statement)
  | SBlock Block (SymbolTable Statement)
  | SWhile Expr Statement (SymbolTable Statement)
  | SIf Expr Statement (SymbolTable Statement)
  | SReturn Expr (SymbolTable Statement)
  deriving (Eq, Ord, Show)

data Expr
 = BOp BinOp Expr Expr (SymbolTable Statement)
 | EAssign Name Expr (SymbolTable Statement)
 | UOp UnOp Expr (SymbolTable Statement)
 | Lit Int
 | Var Name (SymbolTable Statement)
 | Ch Char
  deriving (Eq, Ord, Show)
