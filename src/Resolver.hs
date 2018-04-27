{- |
Module      : Resolver
Description : The resolver annotates each name/var in the AST with its
              corresponding definition. It also renames variables with
              duplicate names so that all name are unique.
Copyright   : (c) Jason Mittertreiner, 2017
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
module Resolver where
import qualified Data.Map as M
import Control.Monad.State.Lazy
import Data.Maybe

import qualified Ast.WeededAst as WA
import qualified Ast.ResolvedAst as RA
import qualified Lib.SymbolTable as ST
import Lib.Types

data ResolveState
  = ResolveState {
    symTab :: ST.SymbolTable
    , renamer :: M.Map Name Name
    , tempNo :: Int
    , varDir :: VarDir
    , modules :: M.Map ModulePath WA.Module
    , currModule :: ModulePath
  }
  deriving (Eq, Ord, Show)

lookupVar :: Name -> Resolver (Name, Def)
lookupVar name = Resolver . state $ \s ->
  case M.lookup name (renamer s) of
    Just n -> case ST.lookup n (symTab s) of
      Just e -> ((n, e), s)
      Nothing -> error $ "Could not find " ++ toString name
        ++ " in symbol table" ++ show s
    Nothing -> case ST.lookup name (symTab s) of
                 Just e -> ((name, e), s)
                 Nothing -> error $ "Could not find " ++ toString name
                   ++ " in renamer " ++ show s

lookupName :: Name -> Resolver QualifiedName
lookupName name = Resolver . state $ \s ->
  case M.lookup name (renamer s) of
    Just n -> case ST.lookupName n (symTab s) of
      Just n -> (n, s)
      Nothing -> error $ "Could not find " ++ toString name
        ++ " in symbol table" ++ show s
    Nothing -> case ST.lookupName name (symTab s) of
                 Just e -> (e, s)
                 Nothing -> error $ "Could not find " ++ toString name
                   ++ " in renamer " ++ show s

{- Adds a unique name declaration to the symbol table. If the name is already
declared, then renames it to be unique
-}
declare :: Name -> Def -> Resolver Name
declare name def = do
  s <- get
  let name' = Name ("t" ++ show (tempNo s) ++ "_" ++ show name)
  modify $ \s -> s {
    symTab = ST.addLocal name' def (symTab s)
    , renamer = M.insert name name' (renamer s)
    , tempNo = tempNo s + 1
    }
  return name'

{- For a given module, returns a map of names that are valid in that module
-}
createModuleScope :: ModulePath -> M.Map ModulePath WA.Module -> M.Map Name (QualifiedName, Def)
createModuleScope mpath globals = case M.lookup mpath globals of
    Just (WA.Module mname imports funcs) -> let
        currModFuncs = let
          fnames = WA.fname <$> funcs
          defs = (\case
                     WA.Func ret name args _ _ -> (mkQName mname name, FuncDef ret (fst <$> args))
                     WA.CFunc ret name args _ -> (mkQName mname name, FuncDef ret (fst <$> args))
                 ) <$> funcs
          in M.fromList $ zip fnames defs
        importFuncs = let
          getImportMod i = fromMaybe
                           (error ("Failed to find import " ++ show i ++ " in " ++ show globals))
                           (M.lookup i globals)
          imptMods = (\m -> (m, getImportMod m)) <$> imports
          in M.fromList $ (\case
                              (mpath, WA.Func ret fname args _ _) -> (fname, (mkQName mpath fname, FuncDef ret (fst <$> args)))
                              (mpath, WA.CFunc ret fname args _) -> (fname, (mkQName mpath fname, FuncDef ret (fst <$> args)))
                          ) <$> (imptMods >>= \(name, mod) -> ((,) name <$> WA.funcs mod))
      in M.union currModFuncs importFuncs
    Nothing -> error ("Failed to find " ++ show mpath ++ " in " ++ show globals ++ "?")

newtype Resolver a = Resolver { resolve :: State ResolveState a }
  deriving (Functor, Applicative, Monad, MonadState ResolveState)

resolver :: M.Map ModulePath WA.Module -> Either String (M.Map ModulePath RA.Prog)
resolver prog =
  forM prog (Right . resolveProg prog)

resolveProg :: M.Map ModulePath WA.Module -> WA.Module-> RA.Prog
resolveProg globals (WA.Module name _ funcs) = let
  globalSymTab = ST.SymbolTable M.empty (createModuleScope name globals)
  in RA.Prog $ (\x -> evalState (resolve (resolveFunc x)) (ResolveState globalSymTab M.empty 0 RVal globals name)) <$> funcs

resolveFunc :: WA.Function -> Resolver RA.Function
resolveFunc (WA.Func tpe name args stmnts exp) = do
  forM_ args (\x -> declare (snd x) (VarDef (fst x)))
  args <- forM args (\arg -> do
                        (name', _) <- lookupVar (snd arg)
                        return (fst arg, name'))
  stmnts' <- forM stmnts resolveStmnt
  exp' <- resolveExpr exp
  currMod <- currModule <$> get
  return $ RA.Func tpe (mkQName currMod name) args stmnts' exp'
resolveFunc (WA.CFunc tpe name args bdy) = do
  currMod <- currModule <$> get
  return $ RA.CFunc tpe (mkQName currMod name) args bdy

inScope :: Resolver a -> Resolver a
inScope action = do
  scope <- get
  ret <- action
  put scope
  return ret

resolveStmnt :: WA.Statement -> Resolver RA.Statement
resolveStmnt (WA.SExpr expr) = RA.SExpr <$> resolveExpr expr
resolveStmnt (WA.SDecl name tpe) = do
  name <- declare name (VarDef tpe)
  return $ RA.SDecl name tpe
resolveStmnt (WA.SDeclAssign name tpe expr) = do
  name <- declare name (VarDef tpe)
  expr' <- resolveExpr expr
  return $ RA.SDeclAssign name tpe expr'
resolveStmnt (WA.SBlock stmnts) = inScope $ RA.SBlock <$> forM stmnts resolveStmnt
resolveStmnt (WA.SWhile expr stmnt) = inScope $ RA.SWhile <$> resolveExpr expr <*> resolveStmnt stmnt
resolveStmnt (WA.SIf expr stmnt) = inScope $ RA.SIf <$> resolveExpr expr <*> resolveStmnt stmnt
resolveStmnt (WA.ForEach name expr stmnt) = inScope $ do
  name' <- declare name (VarDef TUnspec)
  expr' <- resolveExpr expr
  stmnt' <- resolveStmnt stmnt
  return $ RA.ForEach name' expr' stmnt'
resolveStmnt (WA.Kernel k) = RA.Kernel <$> resolveKExpr k

resolveExpr :: WA.Expr -> Resolver RA.Expr
resolveExpr (WA.BOp Access exp1 exp2) = inScope $ do
  modify $ \s -> s{varDir = RVal}
  exp1' <- resolveExpr exp1
  exp2' <- resolveExpr exp2
  return $ RA.BOp Access exp1' exp2'
resolveExpr (WA.BOp op exp1 exp2) = do
  exp1' <- resolveExpr exp1
  exp2' <- resolveExpr exp2
  return $ RA.BOp op exp1' exp2'
resolveExpr (WA.EAssign name expr) = do
  exp' <- resolveExpr expr
  (name, def) <- lookupVar name
  return $ RA.EAssign name def exp'
resolveExpr (WA.EAssignArr e1 e2 e3) = inScope $ do
  modify $ \s -> s{varDir = RVal}
  e1' <- resolveExpr e1
  e2' <- resolveExpr e2
  e3' <- resolveExpr e3
  return $ RA.EAssignArr e1' e2' e3'
resolveExpr (WA.UOp op expr) = RA.UOp op <$> resolveExpr expr
resolveExpr (WA.Lit l sz s) = return $ RA.Lit l sz s
resolveExpr (WA.FLit l sz) = return $ RA.FLit l sz
resolveExpr (WA.ArrLit exprs) = RA.ArrLit <$> forM exprs resolveExpr
resolveExpr (WA.Var oldName) = do
  (name, def) <- lookupVar oldName
  dir <- varDir <$> get
  moduleName <- currModule <$> get
  return $ case def of
    FuncDef _ _ -> RA.FuncName (mkQName moduleName name) def
    VarDef _ -> RA.Var name oldName def dir
    QName _ -> RA.Var name oldName def dir
resolveExpr (WA.Ch c) = return $ RA.Ch c
resolveExpr WA.Unit = return RA.Unit
resolveExpr (WA.Call name exprs) = do
  exprs' <- forM exprs resolveExpr
  (name, def) <- lookupVar name
  qname <- lookupName name
  return $ RA.Call qname def exprs'

resolveKExpr :: WA.KExpr -> Resolver RA.KExpr
resolveKExpr (WA.KBOp op e1 e2) = do
  e1' <- resolveKExpr e1
  e2' <- resolveKExpr e2
  return $ RA.KBOp op e1' e2'
resolveKExpr (WA.KName name) = do
  (name, def) <- lookupVar name
  return $ case def of
    VarDef _ -> RA.KName name def
    _ -> error "Non var in kernel"

