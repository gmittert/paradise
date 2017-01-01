{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Semantic where

import Syntax
import Types
import Control.Monad.State.Lazy

data CodegenState
  = CodegenState {
    symTab :: SymbolTable
    , offset :: Int
    , nextTmp :: Int
  }
  deriving (Eq, Ord, Show)

newtype CodeGen a = CodeGen { genCode :: State CodegenState a }
  deriving (Functor, Applicative, Monad, MonadState CodegenState)

semanticAnalysis :: Prog -> Prog
semanticAnalysis prog = (evalState $ genCode $ initProg prog) (CodegenState emptyTable 0 0)
{-
Rebuilds the syntax tree with the Symbol Tables initialized
-}
initProg :: Prog -> CodeGen Prog
initProg (Prog block _) = do
  beforeState <- gets symTab
  retBlock' <- initRetBlock block
  return $ Prog retBlock' beforeState

{-
Rebuilds a Returning Block with the symbol table initialized
-}
initRetBlock :: RetBlock -> CodeGen RetBlock
initRetBlock (Ret args stmnts expr _) = do
  scope <- gets symTab
  args' <- initArgs args
  stmnts' <- initStatements stmnts
  expr' <- initExpr expr
  return (Ret args' stmnts' expr' scope)

initVoidBlock :: VoidBlock -> CodeGen VoidBlock
initVoidBlock (Void args stmnts _) = do
  scope <- gets symTab
  args' <- initArgs args
  stmnts' <- initStatements stmnts
  return (Void args' stmnts' scope)

initArgs :: Args -> CodeGen Args
initArgs None = return None
initArgs (Args []) = return (Args [])
initArgs (Args (arg:args)) = do
  arg' <- initArg arg
  (Args args') <- initArgs (Args args)
  return $ Args (arg':args')

initArg :: Arg -> CodeGen Arg
initArg (Arg typ name) = do
  modify $ \s -> s{
    symTab = addVar
             name
             (Entry typ (Addr (offset s + toSize typ)))
             (symTab s)
    , offset = offset s + toSize typ
    }
  return $ Arg typ name

initStatements :: Statements -> CodeGen Statements
initStatements (Statements' stmnt _ ) = do
  stmnt' <- initStatement stmnt
  scope <- gets symTab
  return $ Statements' stmnt' scope
initStatements (Statements stmnts stmnt _ ) = do
  stmnts' <- initStatements stmnts
  stmnt' <- initStatement stmnt
  scope <- gets symTab
  return $ Statements stmnts' stmnt' scope

initStatement :: Statement -> CodeGen Statement
initStatement (SAssign name expr _) = do
  scope <- gets symTab
  expr' <- initExpr expr
  return $ SAssign name expr' scope
initStatement (SExpr expr _) = do
  scope <- gets symTab
  return $ SExpr expr scope
initStatement (SPrint expr _) = do
  scope <- gets symTab
  return $ SPrint expr scope
initStatement (SDecl name typ _) = do
  modify $ \s -> s{
    symTab = addVar
              name
              (Entry typ (Addr (offset s + toSize typ)))
              (symTab s)
    , offset = offset s+ toSize typ
    }
  scope <- gets symTab
  return $ SDecl name typ scope
initStatement (SDeclAssign name typ expr _) = do
  modify $ \s -> s{
    symTab = addVar
              name
              (Entry typ (Addr (offset s + toSize typ)))
              (symTab s)
    , offset = offset s + toSize typ
    }
  scope <- gets symTab
  return $ SDeclAssign name typ expr scope
initStatement (SBlock vblock _) = undefined

initExpr :: Expr -> CodeGen Expr
initExpr (Op op name expr _) = do
  scope <- gets symTab
  return $ Op op name expr scope
initExpr (Lit int _) = do
  scope <- gets symTab
  return $ Lit int scope
initExpr (Str str _) = do
  scope <- gets symTab
  return $ Str str scope
initExpr (Var name _) = do
  scope <- gets symTab
  return $ Var name scope
initExpr (Boolean bool _) = do
  scope <- gets symTab
  return $ Boolean bool scope
initExpr (Ch ch _) = do
  scope <- gets symTab
  return $ Ch ch scope
initExpr (EBlock retBlock _) = do
  oldState <- get
  retBlock' <- initRetBlock retBlock
  put oldState
  return $ EBlock retBlock' (symTab oldState)
