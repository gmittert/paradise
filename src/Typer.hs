module Typer where
import           Control.Monad.State.Lazy
import           Control.Monad.Trans.Except
import           Control.Applicative
import qualified Data.Map.Strict as M

import qualified Ast.ResolvedAst as RA
import qualified Ast.TypedAst as TA
import           Lib.Types
import Errors.TypeError
import Errors.CompileError
import qualified Lib.SymbolTable as ST

typer :: M.Map ModulePath RA.Prog -> Either String (M.Map ModulePath TA.Prog)
typer prog = let res = forM prog (\p -> (evalState . TA.runTyper . runExceptT . typeProg) p TA.emptyState)
   in case res of
        Right r -> Right r
        Left l -> Left (Errors.CompileError.toString l)

typeProg :: RA.Prog -> ExceptT TypeError TA.Typer TA.Prog
typeProg (RA.Prog funcs) = TA.Prog <$> forM funcs typeFunc

typeFunc :: RA.Function -> ExceptT TypeError TA.Typer TA.Function
typeFunc (RA.Func tpe name tpes stmnts exp) = withMessage ("While checking function " ++ show name ++ "...") $ do
  stmnts' <- forM stmnts typeStmnt
  exp' <- typeExpr exp
  tpe' <- unify (TA.getExprType exp') tpe
  exp'' <- specType tpe' exp'
  return $ TA.Func tpe name tpes stmnts' exp''

typeStmnt :: RA.Statement -> ExceptT TypeError TA.Typer TA.Statement
typeStmnt (RA.SExpr expr) = TA.SExpr <$> typeExpr expr <*> return Void
typeStmnt (RA.SDecl name tpe) = return $ TA.SDecl name tpe Void
typeStmnt (RA.SDeclAssign name tpe expr) = withMessage ("While checking declaration of " ++ show name ++ " ...") $ do
  typed <- typeExpr expr
  resTpe <- unify tpe (TA.getExprType typed)
  typed' <- specType resTpe typed
  return $ TA.SDeclAssign name resTpe typed' Void

typeStmnt (RA.SBlock stmnts) = TA.SBlock <$> forM stmnts typeStmnt <*> return Void
typeStmnt (RA.SWhile expr stmnt) = do
  expr' <- typeExpr expr
  tpe' <- unify (TA.getExprType expr') (Int I1 Unsigned)
  expr'' <- specType tpe' expr'
  (return $ TA.SWhile expr'') <*> typeStmnt stmnt <*> return Void
typeStmnt (RA.SIf expr stmnt) = do
  expr' <- typeExpr expr
  tpe' <- unify (TA.getExprType expr') (Int I1 Unsigned)
  expr'' <- specType tpe' expr'
  (return $ TA.SIf expr'') <*> typeStmnt stmnt <*> return Void
typeStmnt (RA.ForEach name expr stmnt) = do
  st <- get
  expr' <- typeExpr expr
  let expTpe = TA.getExprType expr'
  case expTpe of
    Arr tpe -> do
      modify $ \s -> s{TA.symTab = ST.addLocal name (VarDef tpe) (TA.symTab s)}
      stmnt' <- typeStmnt stmnt
      put st
      return $ TA.ForEach name expr' stmnt' Void
    _ -> withMessage "Expressions in a foreach loop must be an array" typeError

typeStmnt (RA.Kernel k) = TA.Kernel <$> typeKExpr k <*> return Void

typeNumOp :: BinOp -> TA.Expr -> TA.Expr -> ExceptT TypeError TA.Typer TA.Expr
typeNumOp op e1 e2 = do
  let t1 = TA.getExprType e1
  let t2 = TA.getExprType e2
  resType <- unify t1 t2
  checkNumeric e1
  checkNumeric e2
  return $ TA.BOp op e1 e2 resType

typeCmpOp :: BinOp -> TA.Expr -> TA.Expr -> ExceptT TypeError TA.Typer TA.Expr
typeCmpOp op e1 e2 =
  let t1 = TA.getExprType e1
      t2 = TA.getExprType e2 in
    if t1 `comparable` t2
    then return $ TA.BOp op e1 e2 (Int I1 Unsigned)
    else withContext [e1, e2] $ withMessage ("Cannot " ++ show op ++ " expressions ") typeError

typeExpr :: RA.Expr -> ExceptT TypeError TA.Typer TA.Expr
typeExpr (RA.BOp op exp1 exp2) = withMessage "While checking Binop..." $ do
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
    Assign -> error "Shouldn't see assign here"
    Access -> case TA.getExprType exp1' of
      Arr typ -> do
        checkNumeric exp2'
        return $ TA.BOp op exp1' exp2' typ
      _ -> withContext [exp1'] $ withMessage "Cannot access non array " typeError

typeExpr (RA.EAssign name def expr) = withMessage "While checking assignment... " $ do
  tpe <- case def of
        VarDef tpe -> return tpe
        QName _-> undefined
        FuncDef {} -> withMessage ("Cannot assign " ++ show expr ++ " to function " ++ show name) typeError
  expr' <- typeExpr expr
  resTpe <- unify tpe (TA.getExprType expr')
  withContext [TA.Var name name TUnspec LVal, expr'] $
    specType resTpe (TA.EAssign name expr' resTpe)
typeExpr (RA.ArrLit e3) = withMessage "While checking array literal... " $ do
  exprs <- forM e3 typeExpr
  tpe <- arrType exprs
  withContext exprs $
    specType tpe $ TA.ArrLit exprs tpe
typeExpr (RA.ListComp e) = withMessage "While checking list comprehension..." $ do
  e' <- typeListComp e
  tpe <- unify (TA.getListExprType e') (Arr TUnspec)
  return $ TA.ListComp e' tpe

typeExpr (RA.EAssignArr e1 e2 e3) = withMessage "While checking arr element assignment... " $ do
  e1' <- typeExpr e1 -- Arr[t]
  e2' <- typeExpr e2 -- Int
  e3' <- typeExpr e3 -- t
  checkNumeric e2'
  let arrType = TA.getExprType e1'
  case arrType of
    (Arr elemType) -> withContext [TA.EAssignArr e1' e2' e3' TUnspec, e1', e2', e3'] $ do
      elemType' <- unify elemType (TA.getExprType e3')
      specType elemType' $ TA.EAssignArr e1' e2' e3' elemType'
    _ -> withMessage ("Cannot assign expression of type " ++ show (TA.getExprType e3') ++ " to " ++ show (TA.getExprType e1')) typeError

typeExpr (RA.UOp op expr) = withMessage "While checking unary op... " $ do
  expr' <- typeExpr expr
  case op of
    Not -> case TA.getExprType expr' of
      (Int I1 Unsigned) -> return $ TA.UOp op expr' (Int I1 Unsigned)
      _ -> withContext [expr'] $ withMessage ("Cannot take not of: " ++ show expr) typeError
    Neg -> case TA.getExprType expr' of
      i@(Int _ Signed) -> return $ TA.UOp op expr' i
      _ -> withContext [expr'] $ withMessage ("Cannot negate: " ++ show expr') typeError
    Len -> case TA.getExprType expr' of
      Arr _ -> return $ TA.UOp op expr' (Int I64 Unsigned)
      _ -> withContext [expr'] $ withMessage ("Cannot get length of : " ++ show expr') typeError
    Alloc -> withMessage  "Unexpected alloc while typing" typeError

typeExpr (RA.Lit l sz s)= return $ TA.Lit l sz s
typeExpr RA.Unit = return TA.Unit
typeExpr (RA.FLit l sz)= return $ TA.FLit l sz
typeExpr (RA.Var v vOld def dir) = withMessage ("While checking var... " ++ show vOld) $ do
  tab <- TA.symTab <$> get
  tpe <- case def of
    VarDef tpe -> return tpe
    QName _ -> undefined
    FuncDef _ _ -> withMessage "This shouldn't be a function" typeError
  tpe <- if tpe == TUnspec then
           let def = ST.lookup v tab in
             case def of
               Just (VarDef t) -> return t
               _ -> withMessage ("Couldn't resolve type of " ++ show v) typeError
        else return tpe
  return $ TA.Var v vOld tpe dir
typeExpr (RA.FuncName v def) =
  TA.FuncName v <$> case def of
    VarDef _ -> withMessage "This shouldn't be a var" typeError
    QName _ -> withMessage "This shouldn't be a qname" typeError
    FuncDef res _ -> return res
typeExpr (RA.Ch c) = return $ TA.Ch c
typeExpr (RA.Call var def exprs) = withMessage "While checking call..." $ do
  exprs' <- forM exprs typeExpr
  case def of
    VarDef tpe -> withMessage ("Attempted to call variable " ++ show var ++ " of type " ++ show tpe) typeError
    FuncDef tpe tpes -> do
      let correctNum = length tpes == length exprs
      forM_ (zip (map TA.getExprType exprs') tpes) (uncurry unify)
      if correctNum
      then return $ TA.Call var def exprs' tpe
      else withMessage ("Attempted to call function " ++ show var ++ "(" ++ show tpes ++ ") with " ++ show (map TA.getExprType exprs')) typeError
    QName _ -> undefined
typeExpr (RA.CCall name exprs) = TA.CCall name <$> forM exprs typeExpr

typeKExpr :: RA.KExpr -> ExceptT TypeError TA.Typer TA.KExpr
typeKExpr (RA.KBOp op ke1 ke2) = do
  ke1' <- typeKExpr ke1
  ke2' <- typeKExpr ke2
  if isArr (TA.getKExprType ke1') && TA.getKExprType ke1' == TA.getKExprType ke2' then do
      let tpe' = TA.getKExprType ke1'
      checkNumericArrK ke1'
      return $ TA.KBOp op ke1' ke2' tpe'
    else withMessage ("Kernel called with non array types " ++ show (TA.getKExprType ke1') ++ " and " ++ show (TA.getKExprType ke2') ++ "isArr: " ++ show (isArr (TA.getKExprType ke1')) ++ " eq: " ++ show (ke1' == ke2')) typeError

typeKExpr (RA.KName n def) =
  TA.KName n def <$> case def of
    VarDef tpe -> return tpe
    QName _ -> undefined
    FuncDef _ _ -> withMessage "This shouldn't be a function" typeError

typeListComp :: RA.ListExpr -> ExceptT TypeError TA.Typer TA.ListExpr
typeListComp (RA.LExpr e) = do
  e' <- typeExpr e
  let tpe = TA.getExprType e'
  return $ TA.LExpr e' tpe
typeListComp (RA.LFor e var le) = do
  le' <- typeListComp le
  -- | The thing we're iterating must be an array
  (Arr tpeLe) <- unify (TA.getListExprType le') (Arr TUnspec)
  st <- get
  modify $ \s -> s{TA.symTab = ST.addLocal var (VarDef tpeLe) (TA.symTab s)}
  e' <- typeExpr e
  let tpee' = TA.getExprType e'
  put st
  return $ TA.LFor e' var le' (Arr tpee')
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
Returns the type of an array, or Nothing if the array doesn't have a single
type
-}
arrType :: [TA.Expr] -> ExceptT TypeError TA.Typer Type
arrType arr = do
  let tpes = TA.getExprType <$> arr
  Arr <$> foldM unify (head tpes) tpes

unify :: Type -> Type -> ExceptT TypeError TA.Typer Type
unify TUnspec a = return a
unify a TUnspec = return a
unify (Arr a) (Arr b) = Arr <$> unify a b
unify a b
  | a == b = return a
  | otherwise = case promote a b <|> promote b a of
  Just t -> return t
  Nothing -> withMessage ("Could not unify types: " ++ show a ++ " and " ++ show b) typeError

-- | Fill the type of an expression
specType :: Type -> TA.Expr -> ExceptT TypeError TA.Typer TA.Expr
specType t (TA.BOp Access e1 e2 tpe) = do
  e1' <- specType (Arr t) e1
  tpe' <- unify tpe t
  return (TA.BOp Access e1' e2 tpe')
specType t (TA.BOp op e1 e2 tpe) = do
  e1' <- specType t e1
  e2' <- specType t e2
  tpe' <- unify tpe t
  return (TA.BOp op e1' e2' tpe')
specType t (TA.UOp op e1 tpe) = do
  e1' <- specType t e1
  tpe' <- unify tpe t
  return (TA.UOp op e1' tpe')
specType t (TA.EAssign n e1 tpe) = do
  e1' <- specType t e1
  tpe' <- unify tpe t
  return (TA.EAssign n e1' tpe')
specType t (TA.Lit i sz s) = do
  uni <- unify t (Int sz s)
  case uni of
    (Float sz') -> return $ TA.FLit (fromIntegral i ) sz'
    (Int sz' s') -> return $ TA.Lit i sz' s'
    _ -> withMessage "Unifying Int returned non num?" typeError
specType _ TA.Unit = return TA.Unit
specType t (TA.FLit i sz)  = do
  (Float sz') <- unify t (Float sz)
  return (TA.FLit i sz')
specType t (TA.ArrLit exprs tpe) = do
  (Arr elemType) <- unify t tpe
  exprs' <- forM exprs (specType elemType)
  return (TA.ArrLit exprs' (Arr elemType))
specType t (TA.ListComp e tpe) = do
  e' <- specLExprType t e
  return (TA.ListComp e' tpe)
specType t (TA.Var n nOld tpe dir) = do
  tpe' <- unify tpe t
  return (TA.Var n nOld tpe' dir)
specType t (TA.FuncName n tpe) = TA.FuncName n <$> unify tpe t
specType _ (TA.Ch c) = return $ TA.Ch c
specType t (TA.EAssignArr e1 e2 e3 tpe) = do
  tpe' <- unify tpe t
  e1' <- specType (Arr tpe') e1
  e3' <- specType tpe' e3
  return $ TA.EAssignArr e1' e2 e3' tpe'
specType t (TA.Call n d exprs tpe) = TA.Call n d exprs <$> unify tpe t
specType _ (TA.CCall n exprs) = return $ TA.CCall n exprs

specLExprType :: Type -> TA.ListExpr -> ExceptT TypeError TA.Typer TA.ListExpr
specLExprType t (TA.LExpr e tpe) = do
  tpe' <- unify tpe t
  e' <- specType t e
  return $ TA.LExpr e' tpe'
specLExprType t (TA.LFor e var le tpe) = do
  (Arr tpe') <- unify t tpe
  e' <- specType tpe' e
  return $ TA.LFor e' var le (Arr tpe')
specLExprType t (TA.LRange e1 e2 e3 tpe) = do
  (Arr tpe') <- unify tpe t
  e1' <- specType tpe' e1
  e2' <- specType tpe' e2
  e3' <- specType tpe' e3
  return $ TA.LRange e1' e2' e3' (Arr tpe')

checkNumeric :: TA.Expr -> ExceptT TypeError TA.Typer ()
checkNumeric e =
  if isNumeric (TA.getExprType e)
    then return ()
    else withContext [e] $ withMessage "Expected a numeric expression" typeError

checkNumericArrK :: TA.KExpr -> ExceptT TypeError TA.Typer ()
checkNumericArrK e =
  if isNumericArr (TA.getKExprType e)
    then return ()
    else withMessage "Expected a numeric expression in kernel" typeError

withMessage :: String -> ExceptT TypeError TA.Typer a -> ExceptT TypeError TA.Typer a
withMessage m f = do
  oldMsg <- TA.message <$> get
  modify $ \s -> s{TA.message = m : TA.message s}
  res <- f
  modify $ \s -> s{TA.message = oldMsg}
  return res

withContext :: [TA.Expr] -> ExceptT TypeError TA.Typer a -> ExceptT TypeError TA.Typer a
withContext exps f = do
  oldCtx <- TA.context <$> get
  modify $ \s -> s{TA.context = exps ++ oldCtx}
  res <- f
  modify $ \s -> s{TA.context = oldCtx}
  return res

-- | Throw a type error using the current state
typeError :: ExceptT TypeError TA.Typer a
typeError = do
  msg <- TA.message <$> get
  ctx <- TA.context <$> get
  throwE $ TypeError msg ctx
