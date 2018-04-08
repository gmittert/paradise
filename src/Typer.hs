module Typer where
import           Control.Monad.State.Lazy
import           Control.Monad.Trans.Except
import           Data.List
import qualified Data.Map.Strict as M

import qualified Ast.ResolvedAst as RA
import qualified Ast.TypedAst as TA
import           Lib.Types
import Errors.TypeError
import Errors.CompileError

typer :: M.Map ModulePath RA.Prog -> Either String (M.Map ModulePath TA.Prog)
typer prog = let res = forM prog (\p -> (evalState . TA.runTyper . runExceptT . typeProg) p TA.emptyState)
   in case res of
        Right r -> Right r
        Left l -> Left (Errors.CompileError.toString l)

typeProg :: RA.Prog -> ExceptT TypeError TA.Typer TA.Prog
typeProg (RA.Prog funcs) = TA.Prog <$> forM funcs typeFunc

typeFunc :: RA.Function -> ExceptT TypeError TA.Typer TA.Function
typeFunc (RA.Func tpe name tpes stmnts exp) = do
  stmnts <- typeStmnts stmnts
  exp <- typeExpr exp
  return $ TA.Func tpe name tpes stmnts exp
typeFunc (RA.Proc name tpes stmnts) =
  TA.Proc name tpes <$> typeStmnts stmnts
typeFunc (RA.AsmFunc tpe name tpes bdy) = return $ TA.AsmFunc tpe name tpes bdy

typeStmnts :: RA.Statements -> ExceptT TypeError TA.Typer TA.Statements
typeStmnts (RA.Statements' stmnt) = do
  typed <- typeStmnt stmnt
  return $ TA.Statements' typed Void
typeStmnts (RA.Statements stmnts stmnt) = do
  stmnts' <- typeStmnts stmnts
  stmnt' <- typeStmnt stmnt
  return $ TA.Statements stmnts' stmnt' Void

typeStmnt :: RA.Statement -> ExceptT TypeError TA.Typer TA.Statement
typeStmnt (RA.SExpr expr) = do
  typed <- typeExpr expr
  return $ TA.SExpr typed Void
typeStmnt (RA.SDecl name tpe) = return $ TA.SDecl name tpe Void
typeStmnt (RA.SDeclArr name tpe exprs) = do
  exprs' <- forM exprs typeExpr
  case arrType exprs' of
    Just tpe' -> if tpe /= tpe'
      then throwE $ TypeError ("Type mismatch when assigning to array of type"  ++ show tpe) exprs'
      else return $ TA.SDeclArr name tpe exprs' (Arr tpe')
    Nothing -> throwE (TypeError "Arrays must have a singular type: " exprs')
typeStmnt (RA.SDeclAssign name tpe expr) = do
  typed <- typeExpr expr
  let expTpe = TA.getExprType typed
  if expTpe == tpe
  then return $ TA.SDeclAssign name tpe typed Void
  else throwE $ TypeError ("Expression " ++ show expr ++ " is not of type " ++ show tpe) [typed]

typeStmnt (RA.SBlock stmnts) = do
  stmnts' <- typeStmnts stmnts
  return $ TA.SBlock stmnts' Void
typeStmnt (RA.SWhile expr stmnt) = do
  expr' <- typeExpr expr
  stmnt' <- typeStmnt stmnt
  return $ TA.SWhile expr' stmnt' Void
typeStmnt (RA.SIf expr stmnt) = do
  expr' <- typeExpr expr
  stmnt' <- typeStmnt stmnt
  return $ TA.SIf expr' stmnt' Void

typeNumOp :: BinOp -> TA.Expr -> TA.Expr -> ExceptT TypeError TA.Typer TA.Expr
typeNumOp op e1 e2 = if isNumeric (TA.getExprType e1) && (TA.getExprType e1 == TA.getExprType e2)
                    then return $ TA.BOp op e1 e2 (TA.getExprType e1)
                    else throwE $ TypeError ("Cannot " ++ show op ++ " expressions ") [e1, e2]

typeCmpOp :: BinOp -> TA.Expr -> TA.Expr -> ExceptT TypeError TA.Typer TA.Expr
typeCmpOp op e1 e2 = if TA.getExprType e1 == TA.getExprType e2
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
  if TA.getExprType expr' /= tpe then
      throwE $ TypeError ("Cannot assign " ++ show name ++ " to " ++ show expr) [expr']
      else return (TA.EAssign name expr' tpe)
typeExpr (RA.EAssignArr e1 e2 e3) = do
  e1' <- typeExpr e1
  e2' <- typeExpr e2
  e3' <- typeExpr e3
  case TA.getExprType e1' of
    (Arr tpe) -> if TA.getExprType e3' == tpe && isNumeric (TA.getExprType e2')then
      return (TA.EAssignArr e1' e2' e3' (TA.getExprType e1'))
      else throwE $ TypeError ("Cannot assign expression of type " ++ show (TA.getExprType e3') ++ " to " ++ show (TA.getExprType e1')) []
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
typeExpr (RA.Var v def dir) = do
  tpe <- case def of
    VarDef tpe -> return tpe
    FuncDef _ _ -> throwE $ TypeError "This shouldn't be a function" []
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
    FuncDef tpe tpes -> let
      correctNum = length tpes == length exprs
      correctTypes = all (uncurry (==)) (zip (map TA.getExprType exprs') tpes) in
      if correctNum && correctTypes
      then return $ TA.Call var def exprs' tpe
      else throwE $ TypeError ("Attempted to call function " ++ show var ++ "(" ++ show tpes ++ ") with " ++ show (map TA.getExprType exprs')) []
    QName n -> undefined

{-
Returns the type of an array, or Nothing if the array doesn't have a single
type
-}
arrType :: [TA.Expr] -> Maybe Type
arrType arr = let
  grouped = (group . sort) (TA.getExprType <$> arr)
  in if length grouped == 1
  then (return . Arr . head . head) grouped
  else Nothing
