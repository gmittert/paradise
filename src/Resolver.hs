{- |
Module      : Resolver
Description : The resolver annotates each name/var in the AST with its
              corresponding definition. It also renames variables with
              duplicate names so that all name are unique.
Copyright   : (c) Jason Mittertreiner, 2017
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Resolver where

import Control.Monad.State.Lazy
import Control.Monad.Trans.Except
import Data.Either
import qualified Data.Map as M

import qualified Ast.ResolvedAst as RA
import qualified Ast.WeededAst as WA
import Errors.CompileError
import qualified Lib.SymbolTable as ST
import Lib.Types

data ResolveState = ResolveState
  { symTab :: ST.SymbolTable -- ^A map of names and definitions
  , renamer :: M.Map Name Name -- ^When we see a name shadow, we rename it
  , tempNo :: Int -- ^Unique counter generator for temps
  , varDir :: VarDir -- ^Whether the current name should be an LVal or RVal
  , modules :: M.Map ModulePath WA.Module -- ^The globally defined modules
  , currModule :: ModulePath -- ^The module we are currently resolving
  } deriving (Eq, Ord, Show)

-- |Given a name, get its unique rename and its definition
lookupVar :: Name -> ExceptT CompileError Resolver (Name, Def)
lookupVar name = do
  renamer <- renamer <$> get
  symTab <- symTab <$> get
  case M.lookup name renamer of
    Just n ->
      case ST.lookup n symTab of
        Just e -> return (n, e)
        Nothing ->
          throwE $
          mkResolverE
            ("Could not find " ++
             toString name ++ " in symbol table" ++ show symTab)
            []
            []
    Nothing ->
      case ST.lookup name symTab of
        Just e -> return (name, e)
        Nothing ->
          throwE $
          mkResolverE
            (toString name ++ " is not defined.\n\n" ++ show symTab)
            []
            []

-- |Given a name, get its fully qualified name
lookupName :: Name -> ExceptT CompileError Resolver QualifiedName
lookupName name = do
  renamer <- renamer <$> get
  symTab <- symTab <$> get
  case M.lookup name renamer of
    Just n ->
      case ST.lookupName n symTab of
        Just n -> return n
        Nothing ->
          throwE $
          mkResolverE
            ("Could not find " ++
             toString name ++ " in symbol table" ++ show symTab)
            []
            []
    Nothing ->
      case ST.lookupName name symTab of
        Just e -> return e
        Nothing ->
          throwE $
          mkResolverE
            ("Could not find " ++ toString name ++ " in renamer" ++ show symTab)
            []
            []

-- |Adds a unique name declaration to the symbol table. If the name is already
-- declared, then renames it to be unique
declare :: Name -> Def -> ExceptT CompileError Resolver Name
declare name def = do
  s <- get
  let name' = Name (show name ++ "_" ++ show (tempNo s))
  modify $ \s ->
    s
      { symTab = ST.addLocal name' def (symTab s)
      , renamer = M.insert name name' (renamer s)
      , tempNo = tempNo s + 1
      }
  return name'

-- |For a given module, returns a map of names that are valid in that module
createModuleScope ::
     ModulePath
  -> M.Map ModulePath WA.Module
  -> Either CompileError (M.Map Name (QualifiedName, Def))
createModuleScope mpath globals =
  case globals M.!? mpath of
    Just (WA.Module mname imports cfuncs funcs)
      -- The functions defined in the current module
     ->
      let fnames = WA.fname <$> funcs
          defs = mkEntry mname <$> funcs
          currModFuncs = M.fromList $ zip fnames defs
          cFuncs =
            M.fromList
              (map
                 (\c -> (cname c, (mkQName (ModulePath []) (cname c), CDef c)))
                 cfuncs)
            -- The functions imported by the current module
       in do imprtMods <- mapM (getMod globals) imports
             let imprtFuncs = zip imports (map WA.funcs imprtMods)
             let fnames = concatMap (map WA.fname . snd) imprtFuncs
             let imprtFuncEntries =
                   map (uncurry mkEntry) (flattenMods imprtFuncs)
             let ifuncEntries = M.fromList (zip fnames imprtFuncEntries)
             return $ M.union (M.union currModFuncs ifuncEntries) cFuncs
    Nothing ->
      Left $
      mkResolverE
        ("Failed to find " ++ show mpath ++ " in " ++ show globals)
        []
        []
  where
    mkEntry mname (WA.Func ret name args _ _) =
      (mkQName mname name, FuncDef ret (fst <$> args))
    -- Create a pair of a module path and its module
    getMod globals i =
      case globals M.!? i of
        Just m -> Right m
        Nothing ->
          Left
            (mkResolverE
               ("Failed to find import " ++ show i ++ " in " ++ show globals)
               []
               [])
    flattenMods :: [(a, [b])] -> [(a, b)]
    flattenMods mods = concatMap (\(a, b) -> (map (\x -> (a, x))) b) mods

mkImportDefs ::
     ModulePath
  -> M.Map ModulePath WA.Module
  -> Either CompileError [(QualifiedName, Def)]
mkImportDefs mpath globals =
  case globals M.!? mpath of
    Just (WA.Module _ imports _ _) -> do
      imprtMods <- mapM (getMod globals) imports
      let imprtFuncs = zip imports (map WA.funcs imprtMods)
      let modFuncs = flattenMods imprtFuncs
      let funcs =
            map
              (\(mod, func) ->
                 ( mkQName mod (WA.fname func)
                 , FuncDef (WA.ret func) (map fst (WA.args func))))
              modFuncs
      return funcs
    Nothing ->
      Left $
      mkResolverE
        ("Failed to find " ++ show mpath ++ " in " ++ show globals)
        []
        []
    -- Create a pair of a module path and its module
  where
    getMod globals i =
      case globals M.!? i of
        Just m -> Right m
        Nothing ->
          Left
            (mkResolverE
               ("Failed to find import " ++ show i ++ " in " ++ show globals)
               []
               [])
    flattenMods :: [(a, [b])] -> [(a, b)]
    flattenMods mods = concatMap (\(a, b) -> (map (\x -> (a, x))) b) mods

newtype Resolver a = Resolver
  { resolve :: State ResolveState a
  } deriving (Functor, Applicative, Monad, MonadState ResolveState)

resolver ::
     M.Map ModulePath WA.Module
  -> Either CompileError (M.Map ModulePath RA.Module)
resolver mods = forM mods (resolveModule mods)

resolveModule ::
     M.Map ModulePath WA.Module -> WA.Module -> Either CompileError RA.Module
resolveModule globals (WA.Module name imports cfuncs funcs) = do
  globalSymTab <- ST.SymbolTable M.empty <$> (createModuleScope name globals)
  let res =
        runState
          (forM funcs (\x -> (resolve (runExceptT (resolveFunc x)))))
          (ResolveState globalSymTab M.empty 0 RVal globals name)
  -- Get the funcs if there are no errors
  imprtFuncs <- mkImportDefs name globals
  funcs' <-
    let l = lefts (fst res)
        r = rights (fst res)
     in case l of
          [] -> return r
          (x:_) -> Left x
  let table = (symTab . snd) res
  return (RA.Module name imports imprtFuncs cfuncs funcs' table)

-- |Resolve a function
resolveFunc :: WA.Function -> ExceptT CompileError Resolver RA.Function
resolveFunc (WA.Func tpe name args stmnts exp) = do
  forM_ args (\x -> declare (snd x) (VarDef (fst x)))
  args <-
    forM
      args
      (\arg -> do
         (name', _) <- lookupVar (snd arg)
         return (fst arg, name'))
  stmnts' <- forM stmnts resolveStmnt
  exp' <- resolveExpr exp
  currMod <- currModule <$> get
  return $ RA.Func tpe (mkQName currMod name) args stmnts' exp'

-- |Resolve something in a scope. Once it's finished resolving, the symbol
-- table will be reverted
inScope :: ExceptT CompileError Resolver a -> ExceptT CompileError Resolver a
inScope action = do
  scope <- get
  ret <- action
  put scope
  return ret

-- |Resolve a statement
resolveStmnt :: WA.Statement -> ExceptT CompileError Resolver RA.Statement
resolveStmnt (WA.SExpr expr) = RA.SExpr <$> resolveExpr expr
resolveStmnt (WA.SDecl name tpe) = do
  name <- declare name (VarDef tpe)
  return $ RA.SDecl name tpe
resolveStmnt (WA.SDeclAssign name tpe expr) = do
  name <- declare name (VarDef tpe)
  expr' <- resolveExpr expr
  return $ RA.SDeclAssign name tpe expr'
resolveStmnt (WA.SBlock stmnts) =
  inScope $ RA.SBlock <$> forM stmnts resolveStmnt
resolveStmnt (WA.SWhile expr stmnt) =
  inScope $ RA.SWhile <$> resolveExpr expr <*> resolveStmnt stmnt
resolveStmnt (WA.SIf expr stmnt) =
  inScope $ RA.SIf <$> resolveExpr expr <*> resolveStmnt stmnt
resolveStmnt (WA.ForEach name expr stmnt) = do
  name' <- declare name (VarDef TUnspec)
  expr' <- resolveExpr expr
  stmnt' <- resolveStmnt stmnt
  return $ RA.ForEach name' expr' stmnt'
resolveStmnt (WA.Kernel k) = RA.Kernel <$> resolveKExpr k

-- |Resolve an expression
resolveExpr :: WA.Expr -> ExceptT CompileError Resolver RA.Expr
resolveExpr (WA.BOp Assign e1 e2) =
  inScope $ do
    modify $ \s -> s {varDir = LVal}
    e1' <- resolveExpr e1
    modify $ \s -> s {varDir = RVal}
    e2' <- resolveExpr e2
    return $ RA.BOp Assign e1' e2'
resolveExpr (WA.BOp ArrAccess e1 e2) =
  inScope $ do
    varDir <- gets varDir
    let accessType =
          if varDir == LVal
            then ArrAccessL
            else ArrAccessR
    modify $ \s -> s {varDir = LVal}
    e1' <- resolveExpr e1
    modify $ \s -> s {varDir = RVal}
    e2' <- resolveExpr e2
    return $ RA.BOp accessType e1' e2'
resolveExpr (WA.BOp op exp1 exp2) =
  RA.BOp op <$> resolveExpr exp1 <*> resolveExpr exp2
resolveExpr (WA.UOp op expr) = RA.UOp op <$> resolveExpr expr
resolveExpr (WA.Lit l sz s) = return $ RA.Lit l sz s
resolveExpr (WA.FLit l sz) = return $ RA.FLit l sz
resolveExpr (WA.ArrLit exprs) = RA.ArrLit <$> forM exprs resolveExpr
resolveExpr (WA.ListComp e) = RA.ListComp <$> resolveListComp e
resolveExpr (WA.Var oldName) = do
  (name, def) <- lookupVar oldName
  dir <- varDir <$> get
  moduleName <- currModule <$> get
  return $
    case def of
      FuncDef _ _ -> RA.FuncName (mkQName moduleName name) def
      VarDef _ -> RA.Var name oldName def dir
      QName _ -> RA.Var name oldName def dir
      CDef _ -> RA.FuncName (mkQName (ModulePath []) name) def
resolveExpr (WA.Ch c) = return $ RA.Ch c
resolveExpr WA.Unit = return RA.Unit
resolveExpr c@(WA.Call name exprs) = do
  exprs' <- forM exprs resolveExpr
  (name, def) <- lookupVar name
  qname <- lookupName name
  case def of
    CDef cfunc -> return $ RA.CCall name cfunc exprs'
    FuncDef _ _-> return $ RA.Call qname def exprs'
    _ -> throwE $ mkResolverE (show def ++ "is not a function") [] [c]

-- |Check that we are currently resolving an RVal
checkRVal :: ExceptT CompileError Resolver ()
checkRVal = do
  varDir <- gets varDir
  unless
    (varDir == RVal)
    (throwE $ mkResolverE "Got an RVal while expected an LVal" [] [])

-- |Check that we are currently resolving an LVal
checkLVal :: ExceptT CompileError Resolver ()
checkLVal = do
  varDir <- gets varDir
  unless
    (varDir == LVal)
    (throwE $ mkResolverE "Got an RVal while expected an LVal" [] [])

-- |Resolve a kernel expression
resolveKExpr :: WA.KExpr -> ExceptT CompileError Resolver RA.KExpr
resolveKExpr (WA.KBOp op e1 e2) = do
  e1' <- resolveKExpr e1
  e2' <- resolveKExpr e2
  return $ RA.KBOp op e1' e2'
resolveKExpr (WA.KName name) = do
  (name, def) <- lookupVar name
  case def of
    VarDef _ -> return $ RA.KName name def
    _ -> throwE $ throwInternComp "Non var in kernel"

-- |Resolve a list comprehension
resolveListComp :: WA.ListExpr -> ExceptT CompileError Resolver RA.ListExpr
resolveListComp (WA.LFor e var le) =
  inScope $ do
    le' <- resolveExpr le
    var' <- declare var (VarDef TUnspec)
    e' <- resolveExpr e
    return $ RA.LFor e' var' le'
resolveListComp (WA.LRange e1 e2 e3) = do
  e1' <- resolveExpr e1
  e2' <- resolveExpr e2
  e3' <- resolveExpr e3
  return $ RA.LRange e1' e2' e3'
