{- |
Module      : Resolver
Description : The resolver annotates each name/var in the AST with its
              corresponding definition. It also renames variables with
              duplicate names so that all name are unique.
Copyright   : (c) Jason Mittertreiner, 2017
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Resolver
  ( resolver
  ) where

import Control.Monad.State.Lazy
import Control.Monad.Trans.Except
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

newtype Resolver a = Resolver
  { resolve :: State ResolveState a
  } deriving (Functor, Applicative, Monad, MonadState ResolveState)

-- | Resolve names of the modules
resolver ::
     M.Map ModulePath WA.Module
  -> Either CompileError (M.Map ModulePath RA.Module)
resolver mods = mapM (resolveModule mods) mods

-- | Given a name, get its unique rename and its definition
lookupType :: Name -> ExceptT CompileError Resolver TypeDec
lookupType name = do
  symTab <- symTab <$> get
  case ST.lookupType name symTab of
    Just n -> return n
    Nothing ->
          throwE $
          mkResolverE
            ("Type: " ++ toString name ++ " is not defined.\n\n" ++ show symTab)
            []
            []
lookupTypeCtor :: Name -> ExceptT CompileError Resolver TypeDec
lookupTypeCtor name = do
  symTab <- symTab <$> get
  case ST.lookupTypeCtor name symTab of
    Just n -> return n
    Nothing ->
          throwE $
          mkResolverE
            ("Type contructor: " ++ toString name ++ " is not defined.\n\n" ++ show symTab)
            []
            []

-- | Given a name, get its unique rename and its definition
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

-- | Given a name, get its fully qualified name
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

-- | Adds a unique name declaration to the symbol table. If the name is already
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

-- | Get the AST of a module for a given module path
getModule :: ModulePath -> ExceptT CompileError Resolver WA.Module
getModule i = do
  globals <- gets modules
  maybe
    (throwE
       (mkResolverE
          ("Failed to find the module " ++ show i ++ " in " ++ show globals)
          []
          []))
    return
    (globals M.!? i)

-- | Given a module, get a list of all the modules it imports
getImports :: ModulePath -> ExceptT CompileError Resolver [WA.Module]
getImports mpath = do
  mod <- getModule mpath
  mapM getModule (WA.imports mod)

-- | For a given module, returns a map of type names that are valid in that module
createTypes :: ModulePath -> ExceptT CompileError Resolver (M.Map Name TypeDec)
createTypes mpath = do
  WA.Module {..} <- getModule mpath
  -- The current module type definitions
  imprtTypes <- concatMap WA.typs <$> getImports mpath
  return $ M.fromList $ map (\t -> (typeName t, t)) (typs ++ imprtTypes)

-- | For a given module, returns a map of names that are valid in that module
createModuleScope ::
     ModulePath
  -> ExceptT CompileError Resolver (M.Map Name (QualifiedName, Def))
createModuleScope mpath = do
  WA.Module {..} <- getModule mpath
  -- The current module definitions
  let currModDefs = mkEntry mname <$> funcs
  -- The c definitions imported
  let cDefs = map (\c -> (mkQName (ModulePath []) (cname c), CDef c)) cfuncs
  -- The definitions imported by the current module
  imprtFuncs <- concatMap WA.funcs <$> getImports mpath
  let imprtDefs = mkEntry <$> imports <*> imprtFuncs
  let defs = currModDefs ++ cDefs ++ imprtDefs
  return $ M.fromList (map (\entry -> ((getName . fst) entry, entry)) defs)
  where
    mkEntry mname (WA.Func ret name args _ _ _) =
      (mkQName mname name, FuncDef ret (fst <$> args))

-- | Make definitions for only the imported functions in the module
mkImportDefs ::
     ModulePath -> ExceptT CompileError Resolver [(QualifiedName, Def)]
mkImportDefs mpath = do
  (WA.Module _ imports _ _ _ _) <- getModule mpath
  -- The definitions imported by the current module
  imprtFuncs <- concatMap WA.funcs <$> getImports mpath
  return $ mkEntry <$> imports <*> imprtFuncs
  where
    mkEntry mname (WA.Func ret name args _ _ _) =
      (mkQName mname name, FuncDef ret (fst <$> args))

-- | Resolve a parac module
resolveModule ::
     M.Map ModulePath WA.Module -> WA.Module -> Either CompileError RA.Module
resolveModule globals (WA.Module name imports cfuncs funcs types _) =
  let st = ResolveState mempty M.empty 0 RVal globals name
   in flip evalState st . resolve . runExceptT $
        -- Create the symbol table for the module
       do
        moduleScope <- createModuleScope name
        typeScope <- createTypes name
        modify $ \s -> s {symTab = ST.SymbolTable M.empty moduleScope typeScope}
        -- Resolve the module using the symbol table
        funcs' <- mapM resolveFunc funcs
        imprtFuncs <- mkImportDefs name
        table <- gets symTab
        return (RA.Module name imports imprtFuncs cfuncs funcs' types table)

-- | Resolve a function
resolveFunc :: WA.Function -> ExceptT CompileError Resolver RA.Function
resolveFunc (WA.Func tpe name args stmnts exp _) = inScope $ do
  forM_ args (\(tpe, name) -> declare name (ParamDef $ Just tpe))
  args' <-
    forM
      args
      (\(typ,tname)-> do
         (name', _) <- lookupVar tname
         return (typ, name'))
  stmnts' <- forM stmnts resolveStmnt
  exp' <- resolveExpr exp
  currMod <- currModule <$> get
  return $ RA.Func tpe (mkQName currMod name) args' stmnts' exp'

-- | Resolve something in a scope. Once it's finished resolving, we revert the renamer.
-- This keeps the variables defined in the scope, but new names no longer refer to them
inScope :: ExceptT CompileError Resolver a -> ExceptT CompileError Resolver a
inScope action = do
  rename <- gets renamer
  ret <- action
  -- | Don't lose count of the tempNo
  modify $ \s -> s{renamer = rename}
  return ret

-- |Resolve a statement
resolveStmnt :: WA.Statement -> ExceptT CompileError Resolver RA.Statement
resolveStmnt (WA.SExpr expr _) = RA.SExpr <$> resolveExpr expr
resolveStmnt (WA.SDeclAssign name tpe expr _) = do
  name <- declare name (VarDef tpe)
  expr' <- resolveExpr expr
  return $ RA.SDeclAssign name tpe expr'
resolveStmnt (WA.SBlock stmnts _) =
  inScope $ RA.SBlock <$> forM stmnts resolveStmnt
resolveStmnt (WA.SWhile expr stmnt _) =
  inScope $ RA.SWhile <$> resolveExpr expr <*> resolveStmnt stmnt
resolveStmnt (WA.SIf expr stmnt _) =
  inScope $ RA.SIf <$> resolveExpr expr <*> resolveStmnt stmnt
resolveStmnt (WA.ForEach name expr stmnt _) = do
  name' <- declare name (VarDef Nothing)
  expr' <- resolveExpr expr
  stmnt' <- resolveStmnt stmnt
  return $ RA.ForEach name' expr' stmnt'
resolveStmnt (WA.Kernel k _) = RA.Kernel <$> resolveKExpr k
resolveStmnt (WA.Asm e o i c opt p) = do
  o' <-
    let names = map (flip WA.Var p . snd) o
        strs = map fst o
        resolved = mapM resolveExpr names
     in fmap (zip strs) resolved
  i' <-
    let names = map (flip WA.Var p . snd) i
        strs = map fst i
        resolved = mapM resolveExpr names
     in fmap (zip strs) resolved
  return $ RA.Asm e o' i' c opt p

-- |Resolve an expression
resolveExpr :: WA.Expr -> ExceptT CompileError Resolver RA.Expr
resolveExpr (WA.BOp Assign e1 e2 _) = do
    e1' <- asLVal (resolveExpr e1)
    e2' <- resolveExpr e2
    return $ RA.BOp Assign e1' e2'
resolveExpr (WA.BOp ArrAccess e1 e2 _) = do
    varDir <- gets varDir
    let accessType =
          if varDir == LVal
            then ArrAccessL
            else ArrAccessR
    e1' <- asLVal (resolveExpr e1)
    e2' <- resolveExpr e2
    return $ RA.BOp accessType e1' e2'
resolveExpr (WA.BOp op exp1 exp2 _) =
  RA.BOp op <$> resolveExpr exp1 <*> resolveExpr exp2
resolveExpr (WA.UOp op expr _) = RA.UOp op <$> resolveExpr expr
resolveExpr (WA.Lit l sz s _) = return $ RA.Lit l sz s
resolveExpr (WA.FLit l sz _) = return $ RA.FLit l sz
resolveExpr (WA.ArrLit exprs _) = RA.ArrLit <$> forM exprs resolveExpr
resolveExpr (WA.ListComp e _) = RA.ListComp <$> resolveListComp e
resolveExpr (WA.Var oldName _) = do
  (name, def) <- lookupVar oldName
  dir <- varDir <$> get
  moduleName <- currModule <$> get
  return $
    case def of
      FuncDef _ _ -> RA.FuncName (mkQName moduleName name) def
      VarDef _ -> RA.Var name oldName def dir
      ParamDef _ -> RA.Var name oldName def dir
      QName _ -> RA.Var name oldName def dir
      CDef _ -> RA.FuncName (mkQName (ModulePath []) name) def
resolveExpr (WA.Ch c _) = return $ RA.Ch c
resolveExpr (WA.Unit _) = return RA.Unit
resolveExpr c@(WA.Call name exprs _) = do
  exprs' <- forM exprs resolveExpr
  (name, def) <- lookupVar name
  qname <- lookupName name
  case def of
    CDef cfunc -> return $ RA.CCall name cfunc exprs'
    FuncDef _ _ -> return $ RA.Call qname def exprs'
    _ -> throwE $ mkResolverE (show def ++ "is not a function") [] [c]
resolveExpr (WA.Case e1 patexprs p) = do
  e1' <- resolveExpr e1
  let resolvePatExpr (p, e) = inScope $ do
        p' <- resolvePat p
        e' <- resolveExpr e
        return (p', e')
  patexprs' <- mapM resolvePatExpr patexprs
  return $ RA.Case e1' patexprs' p
resolveExpr (WA.TypeConstr cname exprs posn) = do
  typDec <- lookupTypeCtor cname
  exprs' <- mapM resolveExpr exprs
  return $ RA.TypeConstr {exprs=exprs', ..}

resolvePat :: WA.Pattern -> ExceptT CompileError Resolver RA.Pattern
resolvePat WA.PCh {..} = return $ RA.PCh {..}
resolvePat WA.PLit {..} = return $ RA.PLit {..}
resolvePat WA.PFLit {..} = return $ RA.PFLit {..}
resolvePat (WA.PVar name posn) = do
  name <- declare name (VarDef Nothing)
  return $ RA.PVar {..}
resolvePat (WA.PTypeConstr name pats posn) = do
  typDec <- lookupTypeCtor name
  pats <- mapM resolvePat pats
  return $ RA.PTypeConstr {..}

-- |Resolve a kernel expression
resolveKExpr :: WA.KExpr -> ExceptT CompileError Resolver RA.KExpr
resolveKExpr (WA.KBOp op e1 e2 _) = do
  e1' <- resolveKExpr e1
  e2' <- resolveKExpr e2
  return $ RA.KBOp op e1' e2'
resolveKExpr (WA.KName name _) = do
  (name, def) <- lookupVar name
  case def of
    VarDef _ -> return $ RA.KName name def
    _ -> throwE $ throwInternComp "Non var in kernel"

-- |Resolve a list comprehension
resolveListComp :: WA.ListExpr -> ExceptT CompileError Resolver RA.ListExpr
resolveListComp (WA.LFor e var le _) =
  inScope $ do
    le' <- resolveExpr le
    var' <- declare var (VarDef Nothing)
    e' <- resolveExpr e
    return $ RA.LFor e' var' le'
resolveListComp (WA.LRange e1 e2 e3 _) = do
  e1' <- resolveExpr e1
  e2' <- resolveExpr e2
  e3' <- resolveExpr e3
  return $ RA.LRange e1' e2' e3'

-- | Resolve an expression as producing an lval instead of an rval
asLVal :: ExceptT CompileError Resolver a -> ExceptT CompileError Resolver a
asLVal e = do
  modify $ \s -> s {varDir = LVal}
  e' <- e
  modify $ \s -> s {varDir = RVal}
  return e'
