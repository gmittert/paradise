module Resolver where
import qualified Ast.ParsedAst as PA
import qualified Ast.ResolvedAst as RA
import Control.Monad.State.Lazy

resolver :: PA.Prog -> Either String RA.ResolvedAst
resolver prog = Right $ RA.ResolvedAst $ evalState (RA.resolve (resolveProg prog)) RA.emptyState

resolveProg :: PA.Prog -> RA.Resolver RA.Prog
resolveProg (PA.Prog block) = do
  scope <- get
  block' <- resolveBlock block
  put scope
  return $ RA.Prog block' (RA.symTab scope)

resolveBlock :: PA.Block -> RA.Resolver RA.Block
resolveBlock (PA.Block stmnts) = do
  scope <- get
  stmnts' <- resolveStmnts stmnts
  put scope
  return $ RA.Block stmnts' (RA.symTab scope)

resolveStmnts :: PA.Statements -> RA.Resolver RA.Statements
resolveStmnts (PA.Statements' stmnt) = do
  scope <- get
  stmnt' <- resolveStmnt stmnt
  put scope
  return $ RA.Statements' stmnt' (RA.symTab scope)
resolveStmnts (PA.Statements stmnts stmnt) = do
  scope <- get
  stmnts' <- resolveStmnts stmnts
  stmnt' <- resolveStmnt stmnt
  return $ RA.Statements stmnts' stmnt' (RA.symTab scope)

resolveStmnt :: PA.Statement -> RA.Resolver RA.Statement
resolveStmnt (PA.SExpr expr) = do
  scope <- get
  expr' <- resolveExpr expr
  put scope
  return $ RA.SExpr expr' (RA.symTab scope)
resolveStmnt (PA.SDecl name tpe) = do
  scope <- get
  _ <- RA.insert name tpe (RA.SDecl name tpe (RA.symTab scope))
  return $ RA.SDecl name tpe (RA.symTab scope)
resolveStmnt (PA.SDeclAssign name tpe expr) = do
  scope <- get
  _ <- RA.insert name tpe (RA.SDecl name tpe (RA.symTab scope))
  expr' <- resolveExpr expr
  put scope
  return $ RA.SDeclAssign name tpe expr' (RA.symTab scope)
resolveStmnt (PA.SBlock block) = do
  scope <- get
  block' <- resolveBlock block
  put scope
  return $ RA.SBlock block' (RA.symTab scope)
resolveStmnt (PA.SWhile expr stmnt) = do
  scope <- get
  expr' <- resolveExpr expr
  stmnt' <- resolveStmnt stmnt
  put scope
  return $ RA.SWhile expr' stmnt' (RA.symTab scope)
resolveStmnt (PA.SIf expr stmnt) = do
  scope <- get
  expr' <- resolveExpr expr
  stmnt' <- resolveStmnt stmnt
  put scope
  return $ RA.SIf expr' stmnt' (RA.symTab scope)
resolveStmnt (PA.SReturn expr) = do
  scope <- get
  expr' <- resolveExpr expr
  put scope
  return $ RA.SReturn expr' (RA.symTab scope)

resolveExpr :: PA.Expr -> RA.Resolver RA.Expr
resolveExpr (PA.BOp op exp1 exp2) = do
  scope <- get
  exp1' <- resolveExpr exp1
  exp2' <- resolveExpr exp2
  put scope
  return $ RA.BOp op exp1' exp2' (RA.symTab scope)
resolveExpr (PA.EAssign name expr) = do
  scope <- get
  exp' <- resolveExpr expr
  put scope
  return $ RA.EAssign name exp' (RA.symTab scope)
resolveExpr (PA.UOp op expr) = do
  scope <- get
  exp' <- resolveExpr expr
  put scope
  return $ RA.UOp op exp' (RA.symTab scope)
resolveExpr (PA.Lit l )= return $ RA.Lit l
resolveExpr (PA.Var v) = do
  scope <- get
  return $ RA.Var v (RA.symTab scope)
resolveExpr (PA.Ch c) = return $ RA.Ch c
