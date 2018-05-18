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

typer ::
     M.Map ModulePath RA.Module
  -> Either CompileError (M.Map ModulePath TA.Module)
typer =
  mapM (flip evalState TA.emptyState . TA.runTyper . runExceptT . typeModule)

typeModule :: RA.Module -> ExceptT CompileError TA.Typer TA.Module
typeModule (RA.Module n im imf c f s) = do
  modify $ \st -> st {TA.symTab = s}
  f' <- (forM f typeFunc)
  return $ TA.Module n im imf c f' s

typeFunc :: RA.Function -> ExceptT CompileError TA.Typer TA.Function
typeFunc (RA.Func tpe name tpes stmnts exp) =
  withMessage ("While checking function " ++ show name ++ "...") $ do
    stmnts' <- forM stmnts typeStmnt
    exp' <- typeExpr exp
    tpe' <- unify (TA.getExprType exp') tpe
    exp'' <- specType tpe' exp'
    return $ TA.Func tpe name tpes stmnts' exp''

typeStmnt :: RA.Statement -> ExceptT CompileError TA.Typer TA.Statement
typeStmnt (RA.SExpr expr) = TA.SExpr <$> typeExpr expr <*> return Void
typeStmnt (RA.SDecl name tpe) = return $ TA.SDecl name tpe Void
typeStmnt (RA.SDeclAssign name tpe expr) =
  withMessage ("While checking declaration of " ++ show name ++ "...") $ do
    expr' <- typeExpr expr
    resTpe <- unify tpe (TA.getExprType expr')
    expr'' <- specType resTpe expr'
    tab <- gets TA.symTab
    let tab' = ST.addLocal name (VarDef (TA.getExprType expr'')) tab
    modify $ \s -> s {TA.symTab = tab'}
    return $ TA.SDeclAssign name resTpe expr'' Void
typeStmnt (RA.SBlock stmnts) =
  TA.SBlock <$> forM stmnts typeStmnt <*> return Void
typeStmnt (RA.SWhile expr stmnt) =
  withMessage "While checking while loop... " $ do
    expr' <- withMessage "While checking cond" $ typeExpr expr
    tpe' <- unify (TA.getExprType expr') (Int I1 Unsigned)
    expr'' <- specType tpe' expr'
    return (TA.SWhile expr'') <*>
      (withMessage "While checking body" $ typeStmnt stmnt) <*>
      return Void
typeStmnt (RA.SIf expr stmnt) = do
  expr' <- typeExpr expr
  tpe' <- unify (TA.getExprType expr') (Int I1 Unsigned)
  expr'' <- specType tpe' expr'
  return (TA.SIf expr'') <*> typeStmnt stmnt <*> return Void
typeStmnt (RA.ForEach name expr stmnt) = do
  expr' <- typeExpr expr
  let expTpe = TA.getExprType expr'
  case expTpe of
    Arr _ _ -> do
      stmnt' <- typeStmnt stmnt
      return $ TA.ForEach name expr' stmnt' Void
    _ -> withMessage "Expressions in a foreach loop must be an array" typeError
typeStmnt (RA.Kernel k) = TA.Kernel <$> typeKExpr k <*> return Void

typeNumOp ::
     BinOp -> TA.Expr -> TA.Expr -> ExceptT CompileError TA.Typer TA.Expr
typeNumOp op e1 e2 = do
  let t1 = TA.getExprType e1
  let t2 = TA.getExprType e2
  resType <- unify t1 t2
  checkNumeric e1
  checkNumeric e2
  return $ TA.BOp op e1 e2 resType

typeCmpOp ::
     BinOp -> TA.Expr -> TA.Expr -> ExceptT CompileError TA.Typer TA.Expr
typeCmpOp op e1 e2 =
  withContext [e1, e2] $
  withMessage "While checking cmp" $ do
    let t1 = TA.getExprType e1
        t2 = TA.getExprType e2
     in if t1 `comparable` t2
          then do
            tpe' <- unify t1 t2
            e1' <- specType tpe' e1
            e2' <- specType tpe' e2
            return $ TA.BOp op e1' e2' (Int I1 Unsigned)
          else withContext [e1, e2] $
               withMessage ("Cannot " ++ show op ++ " expressions ") typeError

typeExpr :: RA.Expr -> ExceptT CompileError TA.Typer TA.Expr
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
          resTpe <- unify (TA.getExprType exp1') (TA.getExprType exp2')
          withContext [exp1', exp2'] $
            specType resTpe (TA.BOp Assign exp1' exp2' resTpe)
      ArrAccessL ->
        case TA.getExprType exp1' of
          Arr typ _ -> do
            checkNumeric exp2'
            return $ TA.BOp op exp1' exp2' typ
          _ ->
            withContext [exp1'] $
            withMessage "Cannot access non array " typeError
      ArrAccessR ->
        case TA.getExprType exp1' of
          Arr typ _ -> do
            checkNumeric exp2'
            return $ TA.BOp op exp1' exp2' typ
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
    withContext exprs $ specType tpe $ TA.ArrLit exprs tpe
typeExpr (RA.ListComp e) =
  withMessage "While checking list comprehension..." $ do
    e' <- typeListComp e
    let te@(Arr _ len) = TA.getListExprType e'
    tpe <- unify te (Arr TUnspec len)
    return $ TA.ListComp e' tpe
typeExpr (RA.UOp op expr) =
  withMessage "While checking unary op... " $ do
    expr' <- typeExpr expr
    case op of
      Not ->
        case TA.getExprType expr' of
          (Int I1 Unsigned) -> return $ TA.UOp op expr' (Int I1 Unsigned)
          _ ->
            withContext [expr'] $
            withMessage ("Cannot take not of: " ++ show expr) typeError
      Neg ->
        case TA.getExprType expr' of
          i@(Int _ Signed) -> return $ TA.UOp op expr' i
          _ ->
            withContext [expr'] $
            withMessage ("Cannot negate: " ++ show expr') typeError
      Len ->
        case TA.getExprType expr' of
          Arr _ _ -> return $ TA.UOp op expr' (Int I64 Unsigned)
          _ ->
            withContext [expr'] $
            withMessage ("Cannot get length of : " ++ show expr') typeError
      Alloc -> withMessage "Unexpected alloc while typing" typeError
typeExpr (RA.Lit l sz s) =
  withMessage "While checking lit" $ return $ TA.Lit l sz s
typeExpr RA.Unit = return TA.Unit
typeExpr (RA.FLit l sz) = return $ TA.FLit l sz
typeExpr (RA.Var v vOld def dir) =
  withMessage ("While checking var... " ++ show vOld) $ do
    var_tpe <-
      case def of
        VarDef tpe -> return tpe
        QName _ -> undefined
        FuncDef _ _ -> withMessage "This shouldn't be a function" typeError
        CDef _ -> withMessage "This shouldn't be a function" typeError
    tab <- gets TA.symTab
    let table_tpe = ST.getType v tab
    tpe' <- unify table_tpe var_tpe
    specType tpe' $ TA.Var v vOld tpe' dir
typeExpr (RA.FuncName v def) =
  TA.FuncName v <$>
  case def of
    VarDef _ -> withMessage "This shouldn't be a var" typeError
    QName _ -> withMessage "This shouldn't be a qname" typeError
    FuncDef res _ -> return res
    CDef (CFunc _ tpe _) -> return tpe
typeExpr (RA.Ch c) = withMessage "While checking char" $ return $ TA.Ch c
typeExpr (RA.Call var def exprs) =
  withMessage "While checking call..." $ do
    exprs' <- forM exprs typeExpr
    case def of
      VarDef tpe ->
        withMessage
          (concat
             ["Attempted to call variable ", show var, " of type ", show tpe])
          typeError
      FuncDef tpe tpes -> do
        let correctNum = length tpes == length exprs
        forM_ (zip (map TA.getExprType exprs') tpes) (uncurry unify)
        if correctNum
          then return $ TA.Call var def exprs' tpe
          else withMessage
                 (concat
                    [ "Attempted to call function "
                    , show var
                    , "("
                    , show tpes
                    , ") with "
                    , show (map TA.getExprType exprs')
                    ])
                 typeError
      QName _ -> undefined
      CDef (CFunc _ tpe args) -> do
        let isVarArgs = any (== Varargs) args
        if isVarArgs
          then do
            let dropLast = reverse . tail . reverse
            forM_
              (zip (map TA.getExprType exprs') (dropLast args))
              (uncurry unify)
            return $ TA.Call var def exprs' tpe
          else do
            let correctNum = length args == length exprs
            if correctNum
              then do
                forM_ (zip (map TA.getExprType exprs') args) (uncurry unify)
                return $ TA.Call var def exprs' tpe
              else withMessage
                     (concat
                        [ "Attempted to call function "
                        , show var
                        , "("
                        , show args
                        , ") with "
                        , show (map TA.getExprType exprs')
                        ])
                     typeError
typeExpr (RA.CCall name exprs) = TA.CCall name <$> forM exprs typeExpr

typeKExpr :: RA.KExpr -> ExceptT CompileError TA.Typer TA.KExpr
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
    VarDef tpe -> return tpe
    QName _ -> undefined
    FuncDef _ _ -> withMessage "This shouldn't be a function" typeError
    CDef _ -> withMessage "This shouldn't be a function" typeError

typeListComp :: RA.ListExpr -> ExceptT CompileError TA.Typer TA.ListExpr
typeListComp (RA.LFor e var le) = do
  le' <- typeExpr le
  -- | The thing we're iterating must be an array
  let (Arr tpeLe len) = TA.getExprType le'
  tab <- gets TA.symTab
  modify $ \s -> s {TA.symTab = ST.addLocal var (VarDef tpeLe) tab}
  e' <- typeExpr e
  let tpee' = TA.getExprType e'
  return $ TA.LFor e' var le' (Arr tpee' len)
typeListComp (RA.LRange e1 e2 e3) = do
  e1' <- typeExpr e1
  checkNumeric e1'
  e2' <- typeExpr e2
  checkNumeric e2'
  e3' <- typeExpr e3
  checkNumeric e3'
  tpe <- unify (TA.getExprType e1') (TA.getExprType e2')
  tpe' <- unify tpe (TA.getExprType e3')
  specLExprType tpe' $ TA.LRange e1' e2' e3' tpe'

{-
Returns the type of an array, or and error if the array doesn't have a single
type
-}
arrType :: [TA.Expr] -> ExceptT CompileError TA.Typer Type
arrType arr = do
  let tpes = TA.getExprType <$> arr
  case tpes of
    [] -> Arr <$> foldM unify TUnspec tpes <*> return (length arr)
    (x:_) -> Arr <$> foldM unify x tpes <*> return (length arr)

unify :: Type -> Type -> ExceptT CompileError TA.Typer Type
unify TUnspec a = return a
unify a TUnspec = return a
unify arr1@(Arr a len1) arr2@(Arr b len2)
  | len1 == len2 = Arr <$> unify a b <*> return len1
  | len1 == arrAnyLen = Arr <$> unify a b <*> return len2
  | len2 == arrAnyLen = Arr <$> unify a b <*> return len1
  | otherwise =
    withMessage
      ("Could not unify arrs of different lengths :" ++
       show arr1 ++ ", " ++ show arr2)
      typeError
unify (List a) (List b) = List <$> unify a b
unify a b
  | a == b = return a
  | otherwise =
    case promote a b <|> promote b a of
      Just t -> return t
      Nothing ->
        withMessage
          ("Could not unify types: " ++ show a ++ " and " ++ show b)
          typeError

-- | Fill the type of an expression
specType :: Type -> TA.Expr -> ExceptT CompileError TA.Typer TA.Expr
specType t (TA.BOp ArrAccessL e1 e2 tpe) = do
  e1' <- specType (Arr t arrAnyLen) e1
  tpe' <- unify tpe t
  return (TA.BOp ArrAccessL e1' e2 tpe')
specType t (TA.BOp ArrAccessR e1 e2 tpe) = do
  e1' <- specType (Arr t arrAnyLen) e1
  tpe' <- unify tpe t
  return (TA.BOp ArrAccessR e1' e2 tpe')
specType t (TA.BOp op e1 e2 tpe) = do
  tpe' <- unify tpe t
  return (TA.BOp op e1 e2 tpe')
specType t (TA.UOp op e1 tpe) = do
  e1' <- specType t e1
  tpe' <- unify tpe t
  return (TA.UOp op e1' tpe')
specType t (TA.Lit i sz s) = do
  uni <- unify t (Int sz s)
  case uni of
    (Float sz') -> return $ TA.FLit (fromIntegral i) sz'
    (Int sz' s') -> return $ TA.Lit i sz' s'
    _ -> withMessage "Unifying Int returned non num?" typeError
specType _ TA.Unit = return TA.Unit
specType t (TA.FLit i sz) = do
  (Float sz') <- unify t (Float sz)
  return (TA.FLit i sz')
specType t (TA.ArrLit exprs tpe) = do
  (Arr elemType len) <- unify t tpe
  exprs' <- forM exprs (specType elemType)
  return (TA.ArrLit exprs' (Arr elemType len))
specType t (TA.ListComp e tpe) = do
  e' <- specLExprType t e
  return (TA.ListComp e' tpe)
specType t (TA.Var n nOld tpe dir) = do
  tpe' <- unify tpe t
  tab <- TA.symTab <$> get
  -- Update the symboltable
  case ST.lookup n tab of
    Just (VarDef _) -> do
      let tab' = ST.addLocal n (VarDef tpe') tab
      modify $ \s -> s {TA.symTab = tab'}
    Just (FuncDef _ _) ->
      throwE $ throwInternComp $ "First class functions not yet supported :("
    Just (QName _) -> undefined
    Just (CDef _) ->
      throwE $ throwInternComp $ "First class functions not yet supported :("
    Nothing ->
      throwE $
      throwInternComp $
      "Failed to find var '" ++
      show n ++
      "' in symbol table while specifying types\nNames in scope:\n" ++ show tab
  return (TA.Var n nOld tpe' dir)
specType t (TA.FuncName n tpe) = TA.FuncName n <$> unify tpe t
specType _ (TA.Ch c) = return $ TA.Ch c
specType t (TA.Call n d exprs tpe) = TA.Call n d exprs <$> unify tpe t
specType _ (TA.CCall n exprs) = return $ TA.CCall n exprs

specLExprType ::
     Type -> TA.ListExpr -> ExceptT CompileError TA.Typer TA.ListExpr
specLExprType t (TA.LFor e var le tpe) = do
  (Arr tpe' len) <- unify t tpe
  e' <- specType tpe' e
  return $ TA.LFor e' var le (Arr tpe' len)
specLExprType t (TA.LRange e1 e2 e3 tpe) = do
  (Arr tpe' len) <- unify tpe t
  e1' <- specType tpe' e1
  e2' <- specType tpe' e2
  e3' <- specType tpe' e3
  return $ TA.LRange e1' e2' e3' (Arr tpe' len)

checkNumeric :: TA.Expr -> ExceptT CompileError TA.Typer ()
checkNumeric e =
  unless
    (isNumeric (TA.getExprType e))
    (withContext [e] $ withMessage "Expected a numeric expression" typeError)

checkNumericArrK :: TA.KExpr -> ExceptT CompileError TA.Typer ()
checkNumericArrK e =
  unless
    (isNumericArr (TA.getKExprType e))
    (withMessage "Expected a numeric expression in kernel" typeError)

withMessage ::
     String
  -> ExceptT CompileError TA.Typer a
  -> ExceptT CompileError TA.Typer a
withMessage m f = do
  oldMsg <- TA.message <$> get
  modify $ \s -> s {TA.message = m : TA.message s}
  res <- f
  modify $ \s -> s {TA.message = oldMsg}
  return res

withContext ::
     [TA.Expr]
  -> ExceptT CompileError TA.Typer a
  -> ExceptT CompileError TA.Typer a
withContext exps f = do
  oldCtx <- TA.context <$> get
  modify $ \s -> s {TA.context = exps ++ oldCtx}
  res <- f
  modify $ \s -> s {TA.context = oldCtx}
  return res

-- | Throw a type error using the current state
typeError :: ExceptT CompileError TA.Typer a
typeError = do
  msg <- TA.message <$> get
  ctx <- TA.context <$> get
  throwE $ TyperE $ TypeError msg ctx
