{- |
Module      : Resolver
Description : The resolver annotates each name/var in the AST with its
              corresponding definition. It also renames variables with
              duplicate names so that all name are unique.
Copyright   : (c) Jason Mittertreiner, 2017
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Resolver where
import qualified Ast.WeededAst as WA
import qualified Ast.ResolvedAst as RA
import Control.Monad.State.Lazy
import qualified Lib.SymbolTable as ST
import qualified Data.Map as M
import Lib.Types

data ResolveState
  = ResolveState {
    symTab :: ST.SymbolTable
    , renamer :: M.Map Name Name
    , tempNo :: Int
    , varDir :: VarDir
  }
  deriving (Eq, Ord, Show)

lookupVar :: Name -> Resolver (Name, Def)
lookupVar name = Resolver . state $ \s ->
  case M.lookup name (renamer s) of
    Just n -> case ST.lookup n (symTab s) of
      Just e -> ((n, e), s)
      Nothing -> error $ "Could not find " ++ toString name
        ++ " in symbol table" ++ show s
    Nothing -> case M.lookup name (ST.globals (symTab s)) of
                 Just e -> ((name, e), s)
                 Nothing -> error $ "Could not find " ++ toString name
                   ++ " in renamer " ++ show s

{- Adds a unique name declaration to the symbol table. If the name is already
declared, then renames it to be unique
-}
declare :: Name -> Def -> Resolver Name
declare name def = do
  s <- get
  let name' = Name ("t" ++ show (tempNo s) ++ "(" ++ show name ++ ")")
  modify $ \s -> s {
    symTab = ST.addLocal name' def (symTab s)
    , renamer = M.insert name name' (renamer s)
    , tempNo = tempNo s + 1
    }
  return name'

newtype Resolver a = Resolver { resolve :: State ResolveState a }
  deriving (Functor, Applicative, Monad, MonadState ResolveState)

resolver :: WA.Prog -> Either String RA.Prog
resolver = return . resolveProg

resolveProg :: WA.Prog -> RA.Prog
resolveProg (WA.Prog funcs) = let
  globals = foldr (\x y -> case x of
                      (WA.Func typ name types _) -> ST.addGlobal name (FuncDef typ (map fst types)) y)
            ST.emptyTable funcs
  in RA.Prog $ (\x -> evalState (resolve (resolveFunc x)) (ResolveState globals M.empty 0 RVal)) <$> funcs

resolveFunc :: WA.Function -> Resolver RA.Function
resolveFunc (WA.Func tpe name args stmnts) = do
  _ <- forM args (\x -> declare (snd x) (VarDef (fst x)))
  args <- forM args (\arg -> do
                        (name', _) <- lookupVar (snd arg)
                        return (fst arg, name'))
  stmnts' <- resolveStmnts stmnts
  return $ RA.Func tpe name args stmnts'

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
  name <- declare name (VarDef tpe)
  return $ RA.SDecl name tpe
resolveStmnt (WA.SDeclArr name tpe exprs) = do
  name <- declare name (VarDef tpe)
  exprs <- forM exprs resolveExpr
  return $ RA.SDeclArr name tpe exprs
resolveStmnt (WA.SDeclAssign name tpe expr) = do
  name <- declare name (VarDef tpe)
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
resolveExpr (WA.BOp op exp1 exp2) = do
  exp1' <- resolveExpr exp1
  exp2' <- resolveExpr exp2
  return $ RA.BOp op exp1' exp2'
resolveExpr (WA.EAssign name expr) = do
  exp' <- resolveExpr expr
  (name, def) <- lookupVar name
  return $ RA.EAssign name def exp'
resolveExpr (WA.EAssignArr e1 e2 e3) = do
  scope <- get
  modify $ \s -> s{varDir = LVal}
  e1' <- resolveExpr e1
  modify $ \s -> s{varDir = RVal}
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
resolveExpr (WA.Var name) = do
  (name, def) <- lookupVar name
  dir <- varDir <$> get
  return $ case def of
    FuncDef _ _ -> RA.FuncName name def
    VarDef _ -> RA.Var name def dir

resolveExpr (WA.Ch c) = return $ RA.Ch c
resolveExpr (WA.Call name exprs) = do
  exprs' <- forM exprs resolveExpr
  (name, def) <- lookupVar name
  return $ RA.Call name def exprs'
