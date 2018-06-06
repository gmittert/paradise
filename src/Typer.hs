{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Typer where

import Control.Applicative
import Control.Monad.State.Lazy
import Control.Monad.Trans.Except
import qualified Data.Map.Strict as M

import qualified Ast.ResolvedAst as RA
import qualified Ast.TypedAst as TA
import Errors.CompileError
import Errors.InternalCompileError
import Errors.TypeError
import qualified Lib.SymbolTable as ST
import Lib.Types
import Data.Maybe

data TypeState
  = TypeState {
    symTab :: ST.SymbolTable
    , context :: [TA.Expr]
    , message :: [String]
    , subs :: M.Map Type Type
    , typeVar :: Int
  }
  deriving (Eq, Ord, Show)

emptyState :: TypeState
emptyState = TypeState mempty [] [] M.empty 0

freshTypeVar :: ExceptT CompileError Typer Type
freshTypeVar = do
  count <- gets typeVar
  modify $ \s -> s{typeVar = count +1}
  return $ TypeVar count

newtype Typer a = Typer { runTyper :: State TypeState a }
  deriving (Functor, Applicative, Monad, MonadState TypeState)

typer ::
     M.Map ModulePath RA.Module
  -> Either CompileError (M.Map ModulePath TA.Module)
typer =
  mapM (flip evalState emptyState . runTyper . runExceptT . typeModule)

typeModule :: RA.Module -> ExceptT CompileError Typer TA.Module
typeModule (RA.Module n im imf c f typs s) = do
  modify $ \st -> st {symTab = s}
  f' <- forM f typeFunc
  return $ TA.Module n im imf c f' typs s

typeFunc :: RA.Function -> ExceptT CompileError Typer TA.Function
typeFunc (RA.Func tpe name tpes stmnts exp) =
  withMessage ("While checking function " ++ show name ++ "...") $ do
    stmnts' <- forM stmnts typeStmnt
    exp' <- typeExpr exp
    unify (TA.tpe (exp' :: TA.Expr)) tpe
    exp'' <- subExpr exp'
    return $ TA.Func tpe name tpes stmnts' exp''

updateCtx :: Name -> Def -> ExceptT CompileError Typer ()
updateCtx name def = do
  tab <- gets symTab
  let tab' = ST.addLocal name def tab
  modify $ \s -> s {symTab = tab'}

typeStmnt :: RA.Statement -> ExceptT CompileError Typer TA.Statement
typeStmnt (RA.SExpr expr) = TA.SExpr <$> typeExpr expr <*> return Void
typeStmnt (RA.SDeclAssign name tpe expr) =
  withMessage ("While checking declaration of " ++ show name ++ " : " ++ show tpe ++ "...") $ do
    expr' <- typeExpr expr
    withContext [expr'] $ do
      tpe' <- case tpe of Just tpe -> return tpe; Nothing -> freshTypeVar
      unify tpe' (TA.tpe (expr' :: TA.Expr))
      expr'' <- subExpr expr'
      updateCtx name (VarDef (Just (TA.tpe (expr'' :: TA.Expr))))
      resTpe <- subType tpe'
      return $ TA.SDeclAssign name resTpe expr'' Void
typeStmnt (RA.SBlock stmnts) =
  TA.SBlock <$> forM stmnts typeStmnt <*> return Void
typeStmnt (RA.SWhile expr stmnt) =
  withMessage "While checking while loop... " $ do
    expr' <- withMessage "While checking cond" $ typeExpr expr
    unify (TA.tpe (expr' :: TA.Expr)) (Int I1 Unsigned)
    expr'' <- subExpr expr'
    return (TA.SWhile expr'') <*>
      withMessage "While checking body" (typeStmnt stmnt) <*>
      return Void
typeStmnt (RA.SIf expr stmnt) = withMessage "While checking if" $ do
  expr' <- withMessage "While checking if test" $ typeExpr expr
  unify (TA.tpe (expr' :: TA.Expr)) (Int I1 Unsigned)
  expr'' <- subExpr expr'
  stmnt' <- withMessage "While checking if stm " $ typeStmnt stmnt
  return (TA.SIf expr'' stmnt' Void)
typeStmnt (RA.ForEach name expr stmnt) =
  withMessage "While checking foreach" $ do
  expr' <- typeExpr expr
  let expTpe = TA.tpe (expr' :: TA.Expr)
  case expTpe of
    Arr _ _ -> do
      stmnt' <- typeStmnt stmnt
      return $ TA.ForEach name expr' stmnt' Void
    _ -> withMessage "Expressions in a foreach loop must be an array" typeError
typeStmnt (RA.Kernel k) = TA.Kernel <$> typeKExpr k <*> return Void
typeStmnt (RA.Asm e o i c opt p) = do
  let onames = map snd o
  let ostrs = map fst o
  let inames = map snd i
  let istrs = map fst i
  o' <- mapM typeExpr onames
  i' <- mapM typeExpr inames
  let tpe = case o' of
        [] -> Void
        x:_ -> TA.tpe (x :: TA.Expr)
  return $ TA.Asm e (zip ostrs o') (zip istrs i') c opt p tpe

typeNumOp ::
     BinOp -> TA.Expr -> TA.Expr -> ExceptT CompileError Typer TA.Expr
typeNumOp op e1 e2 = do
  let t1 = TA.tpe (e1 :: TA.Expr)
  let t2 = TA.tpe (e2 :: TA.Expr)
  unify t1 t2
  resType <- subType t1
  e1' <- subExpr e1
  e2' <- subExpr e2
  checkNumeric e1'
  checkNumeric e2'
  return $ TA.BOp op e1' e2' resType

typeCmpOp ::
     BinOp -> TA.Expr -> TA.Expr -> ExceptT CompileError Typer TA.Expr
typeCmpOp op e1 e2 =
  withContext [e1, e2] $
  withMessage "While checking cmp" $ do
    let t1 = TA.tpe (e1 :: TA.Expr)
    let t2 = TA.tpe (e2 :: TA.Expr)
    unify t1 t2
    t1' <- subType t1
    t2' <- subType t2
    if t1' `comparable` t2'
        then TA.BOp op <$> subExpr e1 <*> subExpr e2 <*> return (Int I1 Unsigned)
        else withContext [e1, e2] $
              withMessage ("Cannot " ++ show op ++ " expressions ") typeError

typeExpr :: RA.Expr -> ExceptT CompileError Typer TA.Expr
typeExpr (RA.BOp op exp1 exp2) =
  withMessage "While checking Binop..." $ do
    exp1' <- typeExpr exp1
    exp2' <- typeExpr exp2
    case op of
      Plus -> typeNumOp op exp1' exp2'
      Minus -> typeNumOp op exp1' exp2'
      Times -> typeNumOp op exp1' exp2'
      Div -> typeNumOp op exp1' exp2'
      Lt -> typeNumOp op exp1' exp2'
      Lte -> typeNumOp op exp1' exp2'
      Gt -> typeCmpOp op exp1' exp2'
      Gte -> typeCmpOp op exp1' exp2'
      Eq -> typeCmpOp op exp1' exp2'
      Neq -> typeCmpOp op exp1' exp2'
      Assign ->
        withMessage "While Checking assign ... " $ do
          unify (TA.tpe (exp1' :: TA.Expr)) (TA.tpe (exp2' :: TA.Expr))
          resTpe <- subType (TA.tpe (exp1' :: TA.Expr))
          withContext [exp1', exp2'] $
            subExpr (TA.BOp Assign exp1' exp2' resTpe)
      ArrAccessL ->
        case TA.tpe (exp1' :: TA.Expr) of
          Arr tpe _ -> do
            checkNumeric exp2'
            subExpr (TA.BOp op exp1' exp2' tpe)
          _ ->
            withContext [exp1'] $
            withMessage "Cannot access non array " typeError
      ArrAccessR ->
        case TA.tpe (exp1' :: TA.Expr) of
          Arr tpe _ -> do
            checkNumeric exp2'
            subExpr $ TA.BOp op exp1' exp2' tpe
          _ ->
            withContext [exp1'] $
            withMessage "Cannot access non array " typeError
      ArrAccess ->
        throwE $
        InternalCompileE $
        InternalCompileError "ArrAccess should be already resolved"
typeExpr (RA.ArrLit e3) =
  withMessage "While checking array literal... " $ do
    exprs <- forM e3 typeExpr
    tpe <- arrType exprs
    withContext exprs $ subExpr $ TA.ArrLit exprs tpe
typeExpr (RA.ListComp e) =
  withMessage "While checking list comprehension..." $ do
    e' <- typeListComp e
    let te@(Arr _ len) = TA.getListExprType e'
    tfresh <- freshTypeVar
    unify te (Arr tfresh len)
    TA.ListComp e' <$> subType te
typeExpr (RA.UOp op expr) =
  withMessage "While checking unary op... " $ do
    expr' <- typeExpr expr
    case op of
      Not ->
        case TA.tpe (expr' ::TA.Expr) of
          (Int I1 Unsigned) -> return $ TA.UOp op expr' (Int I1 Unsigned)
          _ ->
            withContext [expr'] $
            withMessage ("Cannot take not of: " ++ show expr) typeError
      Neg ->
        case TA.tpe (expr' :: TA.Expr)of
          i@(Int _ Signed) -> return $ TA.UOp op expr' i
          _ ->
            withContext [expr'] $
            withMessage ("Cannot negate: " ++ show expr') typeError
      Len ->
        case TA.tpe (expr'  :: TA.Expr )of
          Arr _ _ -> return $ TA.UOp op expr' (Int I64 Unsigned)
          Str _ -> return $ TA.UOp op expr' (Int I64 Unsigned)
          _ ->
            withContext [expr'] $
            withMessage ("Cannot get length of : " ++ show expr') typeError
      (Cast t) -> do
        let te = TA.tpe (expr' :: TA.Expr)
        unify t te
        TA.UOp op expr' <$> subType t

typeExpr (RA.Lit l sz s) =
  withMessage "While checking lit" $ return $ TA.Lit l sz s (Int sz s)
typeExpr RA.Unit = return $ TA.Unit Void
typeExpr (RA.FLit l sz) = return $ TA.FLit l sz (Float sz)
typeExpr (RA.Var v vOld def dir) =
  withMessage ("While checking var... " ++ show vOld) $ do
    var_tpe <-
      case def of
        VarDef Nothing -> freshTypeVar
        VarDef (Just tpe) -> return tpe
        ParamDef Nothing -> freshTypeVar
        ParamDef (Just tpe) -> return tpe
        QName _ -> undefined
        FuncDef _ _ -> withMessage "This shouldn't be a function" typeError
        CDef _ -> withMessage "This shouldn't be a function" typeError
    tab <- gets symTab
    table_tpe <- case ST.getType v tab of
        Nothing -> freshTypeVar
        (Just tpe) -> return tpe
    unify table_tpe var_tpe
    tpe' <- subType var_tpe
    updateCtx v (VarDef $ Just tpe')
    subExpr $ TA.Var v vOld def tpe' dir
typeExpr (RA.FuncName v def) =
  TA.FuncName v <$>
  case def of
    VarDef _ -> withMessage "This shouldn't be a var" typeError
    ParamDef _ -> withMessage "This shouldn't be a param" typeError
    QName _ -> withMessage "This shouldn't be a qname" typeError
    FuncDef res _ -> return res
    CDef (CFunc _ tpe _) -> return tpe
typeExpr (RA.Ch c) = return $ TA.Ch c Char
typeExpr (RA.Call var def exprs) =
  withMessage "While checking call..." $ do
    exprs' <- forM exprs typeExpr
    case def of
      FuncDef tpe tpes -> do
        let correctNum = length tpes == length exprs
        forM_ (zip (map (TA.tpe :: TA.Expr -> Type)exprs') tpes) (uncurry unify)
        if correctNum
          then return $ TA.Call var def exprs' tpe
          else withMessage
                 (concat
                    [ "Attempted to call function "
                    , show var
                    , "("
                    , show tpes
                    , ") with "
                    , show (map (TA.tpe :: TA.Expr -> Type)exprs')
                    ])
                 typeError
      _ -> throwE $ throwInternComp "Non function while typing call should be caught in resolver"

typeExpr (RA.CCall var cdef@(CFunc _ tpe args) exprs) =
  withMessage "While checking ccall..." $ do
    exprs' <- forM exprs typeExpr
    let isVarArgs = Varargs `elem` args
    if isVarArgs
      then do
        forM_
          (zip (map (TA.tpe :: TA.Expr -> Type )exprs') (init args))
          (uncurry unify)
        return $ TA.CCall var cdef exprs' tpe
      else do
        let correctNum = length args == length exprs
        if correctNum
          then do
            forM_ (zip (map (TA.tpe :: TA.Expr -> Type ) exprs') args) (uncurry unify)
            return $ TA.CCall var cdef exprs' tpe
          else withMessage
                  (concat
                    [ "Attempted to call function "
                    , show var
                    , "("
                    , show args
                    , ") with "
                    , show (map (TA.tpe :: TA.Expr -> Type) exprs')
                    ])
                  typeError

typeExpr (RA.TypeConstr n dec@(TypeDec tname ctors) exprs p) = do
  exprs' <- mapM typeExpr exprs
  let exptpes = map (TA.tpe :: TA.Expr -> Type ) exprs'
  let (_, tpes) = head $ filter ((== n) . fst) ctors
  mapM_ (uncurry unify) (zip exptpes tpes)
  exprs'' <- mapM subExpr exprs'
  return $ TA.TypeConstr n tpes dec exprs'' p (UserType tname)
typeExpr (RA.Case e patexps p) = do
  e' <- typeExpr e
  pats <- mapM (typePat . fst) patexps
  exprs <- mapM (typeExpr . snd) patexps
  tpe' <- unifyTypes $ (TA.tpe :: TA.Expr -> Type ) <$> exprs
  pattpe <- unifyTypes $ (TA.tpe :: (TA.Pattern -> Type)) <$> pats
  unify pattpe (TA.tpe (e':: TA.Expr))
  return $ TA.Case e' (zip pats exprs) p tpe'

typePat :: RA.Pattern -> ExceptT CompileError Typer TA.Pattern
typePat RA.PCh {..} = return $ TA.PCh{tpe=Char, ..}
typePat RA.PLit {..} = return $ TA.PLit {tpe = Int isz st, ..}
typePat RA.PFLit {..} = return $ TA.PFLit {tpe = Float fsz, ..}
typePat RA.PVar {..} = do
  tvar <- freshTypeVar
  return $ TA.PVar {tpe = tvar, ..}
typePat (RA.PTypeConstr name tdec@(TypeDec tname _) pats posn) = do
  pats' <- mapM typePat pats
  tab <- gets symTab
  case ST.lookupTypeCtorArgs name tab of
    Just targtypes -> do
      mapM_ (uncurry unify) (zip targtypes (map (TA.tpe :: TA.Pattern -> Type) pats'))
      TA.PTypeConstr name tdec <$> mapM subPat pats' <*> return posn <*> return (UserType tname)
    Nothing -> error "Couldn't look up type args"

typeKExpr :: RA.KExpr -> ExceptT CompileError Typer TA.KExpr
typeKExpr (RA.KBOp op ke1 ke2) = do
  ke1' <- typeKExpr ke1
  ke2' <- typeKExpr ke2
  if isArr (TA.getKExprType ke1') &&
     TA.getKExprType ke1' == TA.getKExprType ke2'
    then do
      let tpe' = TA.getKExprType ke1'
      checkNumericArrK ke1'
      return $ TA.KBOp op ke1' ke2' tpe'
    else withMessage
           ("Kernel called with non array types " ++
            show (TA.getKExprType ke1') ++
            " and " ++
            show (TA.getKExprType ke2') ++
            "isArr: " ++
            show (isArr (TA.getKExprType ke1')) ++
            " eq: " ++ show (ke1' == ke2'))
           typeError
typeKExpr (RA.KName n def) =
  TA.KName n def <$>
  case def of
    VarDef (Just tpe) -> return tpe
    VarDef Nothing -> freshTypeVar
    ParamDef (Just tpe) -> return tpe
    ParamDef Nothing -> freshTypeVar
    QName _ -> undefined
    FuncDef _ _ -> withMessage "This shouldn't be a function" typeError
    CDef _ -> withMessage "This shouldn't be a function" typeError

typeListComp :: RA.ListExpr -> ExceptT CompileError Typer TA.ListExpr
typeListComp (RA.LFor e var le) = do
  le' <- typeExpr le
  -- The thing we're iterating must be an array
  let (Arr tpeLe len) = TA.tpe (le' :: TA.Expr)
  tab <- gets symTab
  modify $ \s -> s {symTab = ST.addLocal var (VarDef (Just tpeLe)) tab}
  e' <- typeExpr e
  let tpee' = TA.tpe (e' :: TA.Expr)
  return $ TA.LFor e' var le' (Arr tpee' len)
typeListComp (RA.LRange e1 e2 e3) = do
  e1' <- typeExpr e1
  checkNumeric e1'
  e2' <- typeExpr e2
  checkNumeric e2'
  e3' <- typeExpr e3
  checkNumeric e3'
  unify (TA.tpe (e1' :: TA.Expr)) (TA.tpe (e2' :: TA.Expr))
  tpe' <- subType (TA.tpe (e1' :: TA.Expr))
  unify tpe' (TA.tpe (e3' :: TA.Expr))
  tpe'' <- subType (TA.tpe (e3' :: TA.Expr))
  specLExprType $ TA.LRange e1' e2' e3' tpe''

{-
Returns the type of an array, or and error if the array doesn't have a single
type
-}
arrType :: [TA.Expr] -> ExceptT CompileError Typer Type
arrType arrTpes =
  let tpes = (TA.tpe :: TA.Expr -> Type) <$> arrTpes
  in Arr <$> unifyTypes tpes <*> return (length arrTpes)

unifyTypes :: [Type] -> ExceptT CompileError Typer Type
unifyTypes tpes = do
  tvar <- freshTypeVar
  case tpes of
    [] -> return tvar
    _ -> do
      mapM_ (unify tvar) tpes
      subType tvar

occursFree :: Type -> Type -> Bool
occursFree _ _ = False

unify :: Type -> Type -> ExceptT CompileError Typer ()
unify t1 t2 = do
  unify' t1 t2
  subSymTab
  where
  unify' (TypeVar i) a = if occursFree (TypeVar i) a then
      withMessage "Occurs check failed" typeError
                        else modify $ \s -> s{subs = M.insert (TypeVar i) a (subs s)}
  unify' a (TypeVar i) = if occursFree (TypeVar i) a then
      withMessage "Occurs check failed" typeError
                        else modify $ \s -> s{subs = M.insert (TypeVar i) a (subs s)}
  unify' arr@(Arr Char _) s@(Str _) = unify s arr
  unify' str1@(Str len1) arr1@(Arr Char len2)
    | len1 == len2 = modify $ \s -> s{subs = M.insert str1 arr1 (subs s)}
    | len1 == arrAnyLen = modify $ \s -> s{subs = M.insert str1 arr1 (subs s)}
    | len2 == arrAnyLen = modify $ \s -> s{subs = M.insert str1 arr1 (subs s)}
    | otherwise =
      withMessage
        ("Could not unify arrs of different lengths :" ++
        show arr1 ++ ", " ++ show str1)
        typeError
  unify' arr1@(Arr a len1) arr2@(Arr b len2)
    | len1 == len2 = unify a b
    | len1 == arrAnyLen = unify a b
    | len2 == arrAnyLen = unify a b
    | otherwise =
      withMessage
        ("Could not unify arrs of different lengths :" ++
        show arr1 ++ ", " ++ show arr2)
        typeError
  unify' (List a) (List b) = unify a b
  unify' a b
    | a == b = return ()
    | otherwise =
      case promote a b <|> promote b a of
        Just _ -> return ()
        Nothing ->
          withMessage
            ("Could not unify types: " ++ show a ++ " and " ++ show b)
            typeError

subType :: Type -> ExceptT CompileError Typer Type
subType t = do
  subs <- gets subs
  return $ fromMaybe t (subs M.!? t)

addSub :: Type -> Type -> ExceptT CompileError Typer ()
addSub from to = do
  subs <- gets subs
  -- Add our substitution to the mapping
  case subs M.!? from of
    Just t -> withMessage ("Could not add " ++ show from ++ " to " ++ show to ++ " since " ++ show from ++ " to " ++ show t ++ " already exists") typeError
    Nothing -> modify $ \s -> s {subs = M.insert from to subs}
  -- Update the existing substitutions
  modify $ \s -> s {subs = M.map (\x -> if x == from then to else x) subs}

subSymTab :: ExceptT CompileError Typer ()
subSymTab = do
  ST.SymbolTable{..} <- gets symTab
  locals' <- mapM subDef locals
  modify $ \s -> s{symTab = ST.SymbolTable {ST.locals=locals', ..}}

subDef :: Def -> ExceptT CompileError Typer Def
subDef (FuncDef tpe tpes) = FuncDef <$> subType tpe <*> mapM subType tpes
subDef (VarDef (Just t)) = VarDef . Just <$> subType t
subDef (VarDef Nothing) = return $ VarDef Nothing
subDef (ParamDef (Just t)) = ParamDef . Just <$> subType t
subDef (ParamDef Nothing) = return $ ParamDef Nothing
subDef c@CDef {} = return c
subDef q@QName {} = return q

-- | Fill the type of an expression
subExpr :: TA.Expr -> ExceptT CompileError Typer TA.Expr
subExpr (TA.BOp op e1 e2 tpe) =
  TA.BOp op <$> subExpr e1 <*> subExpr e2 <*> subType tpe
subExpr (TA.UOp op e1 tpe) =
  TA.UOp op <$> subExpr e1 <*> subType tpe
subExpr e@TA.Lit {} = return e
subExpr (TA.Unit t) = return $ TA.Unit t
subExpr e@TA.FLit{} = return e
subExpr (TA.ArrLit exprs tpe) =
  TA.ArrLit <$> mapM subExpr exprs <*> subType tpe
subExpr (TA.ListComp e tpe) =
  TA.ListComp <$> specLExprType e <*> subType tpe
subExpr (TA.Var n nOld def tpe dir) =
  TA.Var n nOld <$> subDef def <*> subType tpe <*> return dir
subExpr (TA.FuncName n tpe) = TA.FuncName n <$> subType tpe
subExpr (TA.Ch c t) = return $ TA.Ch c t
subExpr (TA.Call n d exprs tpe) = TA.Call n d <$> mapM subExpr exprs <*> subType tpe
subExpr (TA.CCall n def exprs tpe) = TA.CCall n def <$> mapM subExpr exprs <*> subType tpe
subExpr (TA.TypeConstr n args typDec exprs posn tpe) =
  TA.TypeConstr n <$> mapM subType args <*> return typDec <*> mapM subExpr exprs <*> return posn <*> subType tpe
subExpr (TA.Case e1 patexps posn tpe) =
  TA.Case <$> subExpr e1 <*> mapM subPatExpr patexps <*> return posn <*> subType tpe

subPatExpr :: (TA.Pattern, TA.Expr) -> ExceptT CompileError Typer (TA.Pattern, TA.Expr)
subPatExpr (pat, exp) = do
  exp' <- subExpr exp
  pat' <- subPat pat
  return (pat', exp')

subPat :: TA.Pattern -> ExceptT CompileError Typer TA.Pattern
subPat TA.PCh{..} = do
  tpe <- subType tpe
  return $ TA.PCh{..}
subPat TA.PLit{..} = do
  tpe <- subType tpe
  return $ TA.PLit{..}
subPat TA.PFLit{..} = do
  tpe <- subType tpe
  return $ TA.PFLit{..}
subPat TA.PVar{..} = do
  tpe <- subType tpe
  return $ TA.PVar{..}
subPat (TA.PTypeConstr name dec pats posn tpe) =
  TA.PTypeConstr name dec <$> mapM subPat pats <*> return posn <*> subType tpe


specLExprType :: TA.ListExpr -> ExceptT CompileError Typer TA.ListExpr
specLExprType (TA.LFor e var le tpe) =
  TA.LFor <$> subExpr e <*> return var <*> return le <*> subType tpe
specLExprType (TA.LRange e1 e2 e3 tpe) =
  TA.LRange <$> subExpr e1 <*> subExpr e2 <*> subExpr e3 <*> subType tpe

checkNumeric :: TA.Expr -> ExceptT CompileError Typer ()
checkNumeric e =
  unless
    (isNumeric (TA.tpe (e :: TA.Expr)))
    (withContext [e] $ withMessage "Expected a numeric expression" typeError)

checkNumericArrK :: TA.KExpr -> ExceptT CompileError Typer ()
checkNumericArrK e =
  unless
    (isNumericArr (TA.getKExprType e))
    (withMessage "Expected a numeric expression in kernel" typeError)

withMessage ::
     String
  -> ExceptT CompileError Typer a
  -> ExceptT CompileError Typer a
withMessage m f = do
  oldMsg <- Typer.message <$> get
  modify $ \s -> s {Typer.message = m : Typer.message s}
  res <- f
  modify $ \s -> s {Typer.message = oldMsg}
  return res

withContext ::
     [TA.Expr]
  -> ExceptT CompileError Typer a
  -> ExceptT CompileError Typer a
withContext exps f = do
  oldCtx <- context <$> get
  modify $ \s -> s {context = exps ++ oldCtx}
  res <- f
  modify $ \s -> s {context = oldCtx}
  return res

-- | Throw a type error using the current state
typeError :: ExceptT CompileError Typer a
typeError = do
  msg <- Typer.message <$> get
  ctx <- context <$> get
  throwE $ TyperE $ TypeError msg ctx
