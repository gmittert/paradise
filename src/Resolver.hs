{- |
Module      : Resolver
Description : The resolver annotates each name/var in the AST with its corresponding definition
Copyright   : (c) Jason Mittertreiner, 2017
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Resolver where
import qualified Ast.WeededAst as WA
import qualified Ast.ResolvedAst as RA
import Control.Monad.State.Lazy
import qualified Lib.SymbolTable as ST
import Types

newtype ResolveState
  = ResolveState {
    symTab :: ST.SymbolTable
  }
  deriving (Eq, Ord, Show)

emptyState :: ResolveState
emptyState = ResolveState ST.emptyTable

lookupVar :: Name -> Resolver Def
lookupVar name = Resolver . state $ \s ->
  case ST.lookup name (symTab s) of
    Just e -> (e,s)
    Nothing -> error $ "Could not find " ++ toString name
      ++ " in table " ++ show s

insert :: Name -> Def -> Resolver ()
insert name def =
  modify $ \s -> s{
    symTab = ST.addLocal name def (symTab s)
    }

newtype Resolver a = Resolver { resolve :: State ResolveState a }
  deriving (Functor, Applicative, Monad, MonadState ResolveState)

resolver :: WA.Prog -> Either String RA.Prog
resolver = return . resolveProg

resolveProg :: WA.Prog -> RA.Prog
resolveProg (WA.Prog funcs) = let
  globals = foldr (\x y -> case x of
                      (WA.Func typ name types _) -> ST.addGlobal name (FuncDef typ types) y)
            ST.emptyTable funcs
  in RA.Prog $ (\x -> evalState (resolve (resolveFunc x)) (ResolveState globals)) <$> funcs

resolveFunc :: WA.Function -> Resolver RA.Function
resolveFunc (WA.Func tpe name tpes stmnts) = do
  stmnts' <- resolveStmnts stmnts
  return $ RA.Func tpe name tpes stmnts'

resolveStmnts :: WA.Statements -> Resolver RA.Statements
resolveStmnts (WA.Statements' stmnt) = do
  stmnt' <- resolveStmnt stmnt
  return $ RA.Statements' stmnt'
resolveStmnts (WA.Statements stmnts stmnt) = do
  stmnts' <- resolveStmnts stmnts
  stmnt' <- resolveStmnt stmnt
  return $ RA.Statements stmnts' stmnt'

resolveStmnt :: WA.Statement -> Resolver RA.Statement
resolveStmnt (WA.SExpr expr) = do
  expr' <- resolveExpr expr
  return $ RA.SExpr expr'
resolveStmnt (WA.SDecl name tpe) = do
  _ <- insert name (VarDef tpe)
  return $ RA.SDecl name tpe
resolveStmnt (WA.SDeclAssign name tpe expr) = do
  _ <- insert name (VarDef tpe)
  expr' <- resolveExpr expr
  return $ RA.SDeclAssign name tpe expr'
resolveStmnt (WA.SBlock stmnts) = do
  scope <- get
  stmnts' <- resolveStmnts stmnts
  put scope
  return $ RA.SBlock stmnts'
resolveStmnt (WA.SWhile expr stmnt) = do
  scope <- get
  expr' <- resolveExpr expr
  stmnt' <- resolveStmnt stmnt
  put scope
  return $ RA.SWhile expr' stmnt'
resolveStmnt (WA.SIf expr stmnt) = do
  scope <- get
  expr' <- resolveExpr expr
  stmnt' <- resolveStmnt stmnt
  put scope
  return $ RA.SIf expr' stmnt'
resolveStmnt (WA.SReturn expr) = do
  scope <- get
  expr' <- resolveExpr expr
  put scope
  return $ RA.SReturn expr'

resolveExpr :: WA.Expr -> Resolver RA.Expr
resolveExpr (WA.BOp op exp1 exp2) = do
  exp1' <- resolveExpr exp1
  exp2' <- resolveExpr exp2
  return $ RA.BOp op exp1' exp2'
resolveExpr (WA.EAssign name expr) = do
  exp' <- resolveExpr expr
  def <- lookupVar name
  return $ RA.EAssign name def exp'
resolveExpr (WA.EAssignArr e1 e2 e3) = do
  scope <- get
  e1' <- resolveExpr e1
  e2' <- resolveExpr e2
  e3' <- resolveExpr e3
  put scope
  return $ RA.EAssignArr e1' e2' e3'
resolveExpr (WA.UOp op expr) = do
  scope <- get
  exp' <- resolveExpr expr
  put scope
  return $ RA.UOp op exp'
resolveExpr (WA.Lit l) = return $ RA.Lit l
resolveExpr (WA.Var v) = do
  def <- lookupVar v
  return $ case def of
    FuncDef _ _ -> RA.FuncName v def
    VarDef _ -> RA.Var v def

resolveExpr (WA.Ch c) = return $ RA.Ch c
resolveExpr (WA.EArr expList) = do
  expList' <- forM expList resolveExpr
  return $ RA.EArr expList'
