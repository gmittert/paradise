module Typer where
import           Control.Monad.State.Lazy
import           Control.Monad.Trans.Except
import           Control.Applicative
import           Data.List
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
typeFunc (RA.Func tpe name tpes stmnts exp) = TA.Func tpe name tpes <$> forM stmnts typeStmnt <*> typeExpr exp
typeFunc (RA.Proc name tpes stmnts) = TA.Proc name tpes <$> forM stmnts typeStmnt
typeFunc (RA.CFunc tpe name tpes bdy) = return $ TA.CFunc tpe name tpes bdy

typeStmnt :: RA.Statement -> ExceptT TypeError TA.Typer TA.Statement
typeStmnt (RA.SExpr expr) = TA.SExpr <$> typeExpr expr <*> return Void
typeStmnt (RA.SDecl name tpe) = return $ TA.SDecl name tpe Void
typeStmnt (RA.SDeclArr name tpe exprs) = do
  exprs' <- forM exprs typeExpr
  elemTypes <- arrType exprs'
  tpe <- unify tpe elemTypes
  return $ TA.SDeclArr name tpe exprs' (Arr tpe)
typeStmnt (RA.SDeclAssign name tpe expr) = do
  typed <- typeExpr expr
  resTpe <- unify tpe (TA.getExprType typed)
  return $ TA.SDeclAssign name resTpe typed Void

typeStmnt (RA.SBlock stmnts) = TA.SBlock <$> forM stmnts typeStmnt <*> return Void
typeStmnt (RA.SWhile expr stmnt) = TA.SWhile <$> typeExpr expr <*> typeStmnt stmnt <*> return Void
typeStmnt (RA.SIf expr stmnt) = TA.SIf <$> typeExpr expr <*> typeStmnt stmnt <*> return Void
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
    _ -> throwE $ TypeError "Expressions in a foreach loop must be an array" [expr']

typeStmnt (RA.Kernel k) = TA.Kernel <$> typeKExpr k <*> return Void

typeNumOp :: BinOp -> TA.Expr -> TA.Expr -> ExceptT TypeError TA.Typer TA.Expr
typeNumOp op e1 e2 = do
  let t1 = TA.getExprType e1
  let t2 = TA.getExprType e2
  resType <- unify t1 t2
  if isNumeric t1 && isNumeric t2
  then return $ TA.BOp op e1 e2 resType
  else throwE $ TypeError ("Cannot " ++ show op ++ " expressions ") [e1, e2]

typeCmpOp :: BinOp -> TA.Expr -> TA.Expr -> ExceptT TypeError TA.Typer TA.Expr
typeCmpOp op e1 e2 =
  let t1 = TA.getExprType e1
      t2 = TA.getExprType e2 in
    if t1 `comparable` t2
    then return $ TA.BOp op e1 e2 Bool
    else throwE $ TypeError ("Cannot " ++ show op ++ " expressions ") [e1, e2]

typeExpr :: RA.Expr -> ExceptT TypeError TA.Typer TA.Expr
typeExpr (RA.BOp op exp1 exp2) = do
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
    Access -> case TA.getExprType exp1' of
      Arr typ -> if isNumeric (TA.getExprType exp2') then
        return $ TA.BOp op exp1' exp2' typ
        else throwE $ TypeError "Cannot access an array with expression " [exp2']
      _ -> throwE $ TypeError "Cannot access non array " [exp1']

typeExpr (RA.EAssign name def expr) = do
  tpe <- case def of
        VarDef tpe -> return tpe
        FuncDef {} -> throwE $ TypeError ("Cannot assign " ++ show expr ++ " to function " ++ show name) []
  expr' <- typeExpr expr
  resTpe <- unify tpe (TA.getExprType expr')
  return (TA.EAssign name expr' resTpe)
typeExpr (RA.EAssignArr e1 e2 e3) = do
  e1' <- typeExpr e1
  e2' <- typeExpr e2
  e3' <- typeExpr e3
  case TA.getExprType e1' of
    (Arr tpe) -> if isNumeric (TA.getExprType e2')then
      TA.EAssignArr e1' e2' e3' <$> unify tpe (TA.getExprType e3') 
      else throwE $ TypeError ("Cannot index with non numeric expression of type " ++ show (TA.getExprType e3') ++ " to " ++ show (TA.getExprType e1')) []
    _ -> throwE $ TypeError ("Cannot assign expression of type " ++ show (TA.getExprType e3') ++ " to " ++ show (TA.getExprType e1')) []

typeExpr (RA.UOp op expr) = do
  expr' <- typeExpr expr
  case op of
    Not -> case TA.getExprType expr' of
      Bool -> return $ TA.UOp op expr' Bool
      _ -> throwE $ TypeError "Cannot take not of: " [expr']
    Neg -> case TA.getExprType expr' of
      i@(Int _ Signed) -> return $ TA.UOp op expr' i
      _ -> throwE $ TypeError "Cannot negate: " [expr']
    Len -> case TA.getExprType expr' of
      Arr _ -> return $ TA.UOp op expr' (Int I64 Unsigned)
      _ -> throwE $ TypeError "Cannot get length of : " [expr']
    Alloc -> throwE $ TypeError "Unexpected alloc while typing" []

typeExpr (RA.Lit l sz s)= return $ TA.Lit l sz s
typeExpr (RA.FLit l sz)= return $ TA.FLit l sz
typeExpr (RA.Var v def dir) = do
  tab <- TA.symTab <$> get
  tpe <- case def of
    VarDef tpe -> return tpe
    FuncDef _ _ -> throwE $ TypeError "This shouldn't be a function" []
  tpe <- if tpe == TUnspec then
           let def = ST.lookup v tab in
             case def of
               Just (VarDef t) -> return t
               _ -> throwE $ TypeError ("Couldn't resolve type of " ++ show v) []
        else return tpe
  return $ TA.Var v tpe dir
typeExpr (RA.FuncName v def) = do
  tpe <- case def of
    VarDef _ -> throwE $ TypeError "This shouldn't be a var" []
    QName _ -> throwE $ TypeError "This shouldn't be a qname" []
    FuncDef res _ -> return res
  return $ TA.FuncName v tpe
typeExpr (RA.Ch c) = return $ TA.Ch c
typeExpr (RA.Call var def exprs) = do
  exprs' <- forM exprs typeExpr
  case def of
    VarDef tpe -> throwE $ TypeError ("Attempted to call variable " ++ show var ++ " of type " ++ show tpe) []
    FuncDef tpe tpes -> do
      let correctNum = length tpes == length exprs
      forM_ (zip (map TA.getExprType exprs') tpes) (uncurry unify)
      if correctNum
      then return $ TA.Call var def exprs' tpe
      else throwE $ TypeError ("Attempted to call function " ++ show var ++ "(" ++ show tpes ++ ") with " ++ show (map TA.getExprType exprs')) []
    QName n -> undefined

typeKExpr (RA.KBOp op ke1 ke2) = do
  ke1' <- typeKExpr ke1
  ke2' <- typeKExpr ke2
  if isArr (TA.getKExprType ke1') && (TA.getKExprType ke1') == (TA.getKExprType ke2') then
    let (Arr t) = TA.getKExprType ke1' in
      if isNumeric t then return $ TA.KBOp op ke1' ke2' (Arr t)
      else throwE $ TypeError ("Kernel called with non numeric types " ++ show (TA.getKExprType ke1') ++ " and " ++ show (TA.getKExprType ke2')) []
    else throwE $ TypeError ("Kernel called with non array types " ++ show (TA.getKExprType ke1') ++ " and " ++ show (TA.getKExprType ke2') ++ "isArr: " ++ show (isArr (TA.getKExprType ke1')) ++ " eq: " ++ show (ke1' == ke2')) []

typeKExpr (RA.KName n def) = do
  tpe <- case def of
    VarDef tpe -> return tpe
    FuncDef _ _ -> throwE $ TypeError "This shouldn't be a function" []
  return $ TA.KName n def tpe

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
  Nothing -> throwE $ TypeError ("Could not unify types: " ++ show a ++ " and " ++ show b) []
