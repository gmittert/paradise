module Typer where
import qualified Ast.ResolvedAst as RA
import qualified Ast.TypedAst as TA
import Types
import qualified Data.Map.Strict as M
import Lib.SymbolTable
import Control.Monad.State.Lazy
import Control.Monad.Trans.Except
import Data.List

typer :: RA.Prog -> Either String TA.Prog
typer p = (evalState . TA.runTyper . runExceptT . typeProg) p TA.emptyState

typeProg :: RA.Prog -> ExceptT String TA.Typer TA.Prog
typeProg (RA.Prog funcs) = do
  funcs' <- forM funcs typeFunc
  return $ TA.Prog funcs'

typeFunc (RA.Func tpe name tpes stmnts) = do
  stmnts' <- typeStmnts stmnts
  return $ TA.Func tpe name tpes stmnts'

typeStmnts :: RA.Statements -> ExceptT String TA.Typer TA.Statements
typeStmnts (RA.Statements' stmnt) = do
  typed <- typeStmnt stmnt
  table <- TA.symTab <$> lift get
  return $ TA.Statements' typed Void
typeStmnts (RA.Statements stmnts stmnt) = do
  stmnts' <- typeStmnts stmnts
  stmnt' <- typeStmnt stmnt
  table <- TA.symTab <$> lift get
  return $ TA.Statements stmnts' stmnt' Void

typeStmnt :: RA.Statement -> ExceptT String TA.Typer TA.Statement
typeStmnt (RA.SExpr expr) = do
  typed <- typeExpr expr
  return $ TA.SExpr typed Void
typeStmnt (RA.SDecl name tpe) = return $ TA.SDecl name tpe Void
typeStmnt (RA.SDeclAssign name tpe expr) = do
  typed <- typeExpr expr
  table <- TA.symTab <$> lift get
  let expTpe = TA.getExprType typed
  if expTpe == tpe
  then return $ TA.SDeclAssign name tpe typed Void
  else throwE $ "Expression " ++ show expr ++ " is not of type " ++ show tpe

typeStmnt (RA.SBlock stmnts) = do
  stmnts' <- typeStmnts stmnts
  table <- TA.symTab <$> lift get
  return $ TA.SBlock stmnts' Void
typeStmnt (RA.SWhile expr stmnt) = do
  expr' <- typeExpr expr
  stmnt' <- typeStmnt stmnt
  table <- TA.symTab <$> lift get
  return $ TA.SWhile expr' stmnt' Void
typeStmnt (RA.SIf expr stmnt) = do
  expr' <- typeExpr expr
  stmnt' <- typeStmnt stmnt
  table <- TA.symTab <$> lift get
  return $ TA.SIf expr' stmnt' Void
typeStmnt (RA.SReturn expr) = do
  expr' <- typeExpr expr
  table <- TA.symTab <$> lift get
  return $ TA.SReturn expr' Void

typeExpr :: RA.Expr -> ExceptT String TA.Typer TA.Expr
typeExpr (RA.BOp op exp1 exp2) = do
  exp1' <- typeExpr exp1
  exp2' <- typeExpr exp2
  case op of
    Plus -> if (TA.getExprType exp1' /= Int) || (TA.getExprType exp2' /= Int) then
      throwE $ "Type error: Cannot add expressions" ++ show exp1 ++ ", " ++ show exp2
      else return (TA.BOp op exp1' exp2' Int)
    Minus -> if (TA.getExprType exp1' /= Int) || (TA.getExprType exp2' /= Int) then
      throwE $ "Type error: Cannot subtract expressions" ++ show exp1 ++ ", " ++ show exp2
      else return (TA.BOp op exp1' exp2' Int)
    Times -> if (TA.getExprType exp1' /= Int) || (TA.getExprType exp2' /= Int) then
      throwE $ "Type error: Cannot multiply expressions" ++ show exp1 ++ ", " ++ show exp2
      else return (TA.BOp op exp1' exp2' Int)
    Div -> if (TA.getExprType exp1' /= Int) || (TA.getExprType exp2' /= Int) then
      throwE $ "Type error: Cannot divide expressions" ++ show exp1 ++ ", " ++ show exp2
      else return (TA.BOp op exp1' exp2' Int)
    Lt -> if (TA.getExprType exp1' /= Int) || (TA.getExprType exp2' /= Int) then
      throwE $ "Type error: Cannot compare expressions" ++ show exp1 ++ " to " ++ show exp2
      else return (TA.BOp op exp1' exp2' Int)
    Lte -> if TA.getExprType exp1' /= Int || (TA.getExprType exp2' /= Int) then
      throwE $ "Type error: Cannot compare expressions" ++ show exp1 ++ " to " ++ show exp2
      else return $ TA.BOp op exp1' exp2' Int
    Access -> case TA.getExprType exp1' of
      Arr typ _ -> if TA.getExprType exp2' == Int then
        return $ TA.BOp op exp1' exp2' typ
        else throwE $ "Type error: Cannot access and array with expression " ++ show exp2
      _ -> throwE $ "Type error: cannot access non array " ++ show exp1

typeExpr (RA.EAssign name def expr) = do
  tpe <- case def of
        VarDef tpe -> return tpe
        FuncDef {} -> throwE $ "Type error: Cannot assign " ++ show expr ++ " to function " ++ show name
  expr' <- typeExpr expr
  if (TA.getExprType expr') /= tpe then
      throwE $ "Type error: Cannot assign " ++ show name ++ " to " ++ show expr
      else return (TA.EAssign name expr' tpe)
typeExpr (RA.EAssignArr e1 e2 e3) = do
  e1' <- typeExpr e1
  e2' <- typeExpr e2
  e3' <- typeExpr e3
  case TA.getExprType e1' of
    (Arr tpe _) -> if TA.getExprType e3' == tpe && TA.getExprType e2' == Int then
      return (TA.EAssignArr e1' e2' e3' (TA.getExprType e1'))
      else throwE $ "Type error: Cannot assign expression of type " ++ show (TA.getExprType e3') ++ " to " ++ show (TA.getExprType e1')
    _ -> throwE $ "Type error: Cannot assign expression of type " ++ show (TA.getExprType e3') ++ " to " ++ show (TA.getExprType e1')

typeExpr (RA.UOp op expr) = do
  expr' <- typeExpr expr
  case op of
    Deref -> case TA.getExprType expr' of
      Pointer tpe -> return $ TA.UOp op expr' tpe
      _ -> throwE $ "Cannot dereference non pointer: " ++ show expr
    Neg -> case TA.getExprType expr' of
      Int -> return $ TA.UOp op expr' Int
      _ -> throwE $ "Cannot negate: " ++ show expr

typeExpr (RA.Lit l)= return $ TA.Lit l
typeExpr (RA.Var v def) = do
  tpe <- case def of
    VarDef tpe -> return tpe
    FuncDef res _ -> return res
  return $ TA.Var v tpe
typeExpr (RA.Ch c) = return $ TA.Ch c
typeExpr (RA.EArr exprs) = do
  exprs' <- forM exprs typeExpr
  table <- TA.symTab <$> lift get
  case arrType exprs' of
    Just tpe -> return $ TA.EArr exprs' (Arr tpe (length exprs))
    Nothing -> throwE "Arrays must have a singular type"

{-
Returns the type of an array, or Nothing if the array doesn't have a single
type
-}
arrType :: [TA.Expr] -> Maybe Type
arrType arr = let
  grouped = (group . sort) arr
  in if length grouped == 1
  then (return . TA.getExprType . head . head) grouped
  else Nothing
