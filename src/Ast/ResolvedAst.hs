{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Ast.ResolvedAst where
import Types
import qualified Data.Map.Strict as M
import Control.Monad.State.Lazy

data Entry = Entry Type Statement
  deriving (Eq, Ord, Show)

data SymbolTable = SymbolTable {
  vars :: M.Map Name Entry
  , funcs :: M.Map Name Entry
  } deriving (Eq, Ord, Show)

emptyTable :: SymbolTable
emptyTable = SymbolTable M.empty M.empty

addVar :: Name -> Entry -> SymbolTable -> SymbolTable
addVar name entry scope = scope {
  vars = M.insert name entry (vars scope)
}

addFunc :: Name -> Entry -> SymbolTable -> SymbolTable
addFunc name entry scope = scope {
  funcs = M.insert name entry (funcs scope)
}

lookup :: Name -> Resolver Entry
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
    symTab :: SymbolTable
  }
  deriving (Eq, Ord, Show)

emptyState :: ResolveState
emptyState = ResolveState emptyTable

newtype Resolver a = Resolver { resolve :: State ResolveState a }
  deriving (Functor, Applicative, Monad, MonadState ResolveState)

data ResolvedAst = ResolvedAst Prog

data Prog
  = Prog Block SymbolTable
  deriving(Eq, Ord, Show)

data Block = Block Statements SymbolTable
  deriving (Eq, Ord, Show)

data Statements
 = Statements' Statement SymbolTable
 | Statements Statements Statement SymbolTable
  deriving (Eq, Ord, Show)

data Statement
  = SExpr Expr SymbolTable
  | SDecl Name Type SymbolTable
  | SDeclAssign Name Type Expr SymbolTable
  | SBlock Block SymbolTable
  | SWhile Expr Statement SymbolTable
  | SIf Expr Statement SymbolTable
  | SReturn Expr SymbolTable
  deriving (Eq, Ord, Show)

data Expr
 = BOp BinOp Expr Expr SymbolTable
 | EAssign Name Expr SymbolTable
 | UOp UnOp Expr SymbolTable
 | Lit Int
 | Var Name SymbolTable
 | Ch Char
  deriving (Eq, Ord, Show)
