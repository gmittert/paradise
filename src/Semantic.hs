module Semantic where

import Syntax
import Types
import Control.Monad.State.Lazy

semanticAnalysis :: Prog -> Prog
semanticAnalysis prog = (evalState $ genCode $ initProg prog) (CodegenState emptyTable 0 0)
{-
Rebuilds the syntax tree with the Symbol Tables initialized
-}
initProg :: Prog -> CodeGen Prog
initProg (Prog block _) = do
  scope <- get
  retBlock' <- initBlock block
  put scope
  return $ Prog retBlock' scope

initBlock :: Block -> CodeGen Block
initBlock (Block stmnts _) = do
  parentScope <- get
  stmnts' <- initStatements stmnts
  scope <- get
  put parentScope
  return (Block stmnts' scope)

initArgs :: Args -> CodeGen Args
initArgs None = return None
{-
- If we have arguments, we want to wipe out the old scope
-}
initArgs args = do
  put (CodegenState emptyTable 0 0)
  initArgs' args

initArgs' :: Args -> CodeGen Args
initArgs' None = undefined -- We've checked for this match in initArgs
initArgs' (Args []) = return (Args [])
initArgs' (Args (arg:args)) = do
  arg' <- initArg arg
  (Args args') <- initArgs' (Args args)
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
initStatements (Statements' stmnt _) = do
  stmnt' <- initStatement stmnt
  scope <- get
  return $ Statements' stmnt' scope
initStatements (Statements stmnts stmnt _) = do
  stmnts' <- initStatements stmnts
  stmnt' <- initStatement stmnt
  scope <- get
  return $ Statements stmnts' stmnt' scope

initStatement :: Statement -> CodeGen Statement
initStatement (SExpr expr _) = do
  scope <- get
  return $ SExpr expr scope
initStatement (SPrint expr _) = do
  scope <- get
  return $ SPrint expr scope
initStatement (SDecl name typ _) = do
  modify $ \s -> s{
    symTab = addVar
              name
              (Entry typ (Addr (offset s + toSize typ)))
              (symTab s)
    , offset = offset s+ toSize typ
    }
  scope <- get
  return $ SDecl name typ scope
initStatement (SDeclAssign name typ expr _) = do
  modify $ \s -> s{
    symTab = addVar
              name
              (Entry typ (Addr (offset s + toSize typ)))
              (symTab s)
    , offset = offset s + toSize typ
    }
  scope <- get
  return $ SDeclAssign name typ expr scope
initStatement (SBlock vblock _) = do
  parentScope <- get
  block' <- initBlock vblock
  scope <- get
  put parentScope
  return $ SBlock block' scope

initExpr :: Expr -> CodeGen Expr
initExpr (BOp op name expr _) = do
  scope <- get
  return $ BOp op name expr scope
initExpr (Lit int _) = do
  scope <- get
  return $ Lit int scope
initExpr (Str str _) = do
  scope <- get
  return $ Str str scope
initExpr (Var name _) = do
  scope <- get
  return $ Var name scope
initExpr (Boolean bool _) = do
  scope <- get
  return $ Boolean bool scope
initExpr (Ch ch _) = do
  scope <- get
  return $ Ch ch scope
