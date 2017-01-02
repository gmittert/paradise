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
  retBlock' <- initRetBlock block
  put scope
  return $ Prog retBlock' scope

{-
Rebuilds a Returning Block with the symbol table initialized
-}
initRetBlock :: RetBlock -> CodeGen RetBlock
initRetBlock (Ret args stmnts expr _) = do
  scope <- get
  args' <- initArgs args
  stmnts' <- initStatements stmnts
  expr' <- initExpr expr
  put scope
  return (Ret args' stmnts' expr' scope)

initVoidBlock :: VoidBlock -> CodeGen VoidBlock
initVoidBlock (Void args stmnts _) = do
  scope <- get
  args' <- initArgs args
  stmnts' <- initStatements stmnts
  put scope
  return (Void args' stmnts' scope)

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
initStatements (Statements' stmnt _ ) = do
  scope <- get
  stmnt' <- initStatement stmnt
  return $ Statements' stmnt' scope
initStatements (Statements stmnts stmnt _ ) = do
  scope <- get
  stmnts' <- initStatements stmnts
  stmnt' <- initStatement stmnt
  return $ Statements stmnts' stmnt' scope

initStatement :: Statement -> CodeGen Statement
initStatement (SAssign name expr _) = do
  scope <- get
  expr' <- initExpr expr
  return $ SAssign name expr' scope
initStatement (SExpr expr _) = do
  scope <- get
  return $ SExpr expr scope
initStatement (SPrint expr _) = do
  scope <- get
  return $ SPrint expr scope
initStatement (SDecl name typ _) = do
  scope <- get
  modify $ \s -> s{
    symTab = addVar
              name
              (Entry typ (Addr (offset s + toSize typ)))
              (symTab s)
    , offset = offset s+ toSize typ
    }
  return $ SDecl name typ scope
initStatement (SDeclAssign name typ expr _) = do
  scope <- get
  modify $ \s -> s{
    symTab = addVar
              name
              (Entry typ (Addr (offset s + toSize typ)))
              (symTab s)
    , offset = offset s + toSize typ
    }
  return $ SDeclAssign name typ expr scope
initStatement (SBlock vblock _) = do
  scope <- get
  vblock' <- initVoidBlock vblock
  put scope
  return $ SBlock vblock' scope

initExpr :: Expr -> CodeGen Expr
initExpr (Op op name expr _) = do
  scope <- get
  return $ Op op name expr scope
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
initExpr (EBlock retBlock _) = do
  scope <- get
  retBlock' <- initRetBlock retBlock
  return $ EBlock retBlock' scope
