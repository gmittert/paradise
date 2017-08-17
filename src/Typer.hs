module Typer where
import qualified Ast.ResolvedAst as RA
import qualified Ast.TypedAst as TA
import Types
import qualified Data.Map.Strict as M
import Lib.SymbolTable
import Control.Monad.State.Lazy
import Control.Monad.Trans.Except

typer :: RA.ResolvedAst -> Either String TA.TypedAst
typer (RA.ResolvedAst prog) = TA.TypedAst <$> (evalState . TA.runTyper . runExceptT . typeProg) prog TA.emptyState

typeProg :: RA.Prog -> ExceptT String TA.Typer TA.Prog
typeProg (RA.Prog block _) = do
  block'@(TA.Block _ table _) <- typeBlock block
  return $ TA.Prog block' table Int

typeBlock :: RA.Block -> ExceptT String TA.Typer TA.Block
typeBlock (RA.Block stmnts _) = do
  typed <- typeStmnts stmnts
  table <- TA.symTab <$> lift get
  return $ TA.Block typed table Void

typeStmnts :: RA.Statements -> ExceptT String TA.Typer TA.Statements
typeStmnts (RA.Statements' stmnt _) = do
  typed <- typeStmnt stmnt
  table <- TA.symTab <$> lift get
  return $ TA.Statements' typed table Void
typeStmnts (RA.Statements stmnts stmnt _) = do
  stmnts' <- typeStmnts stmnts
  stmnt' <- typeStmnt stmnt
  table <- TA.symTab <$> lift get
  return $ TA.Statements stmnts' stmnt' table Void

typeStmnt :: RA.Statement -> ExceptT String TA.Typer TA.Statement
typeStmnt (RA.SExpr expr _) = do
  typed <- typeExpr expr
  return $ TA.SExpr typed (TA.getExprTable typed) Void
typeStmnt (RA.SDecl name tpe _) = return $ TA.SDecl name tpe undefined Void
typeStmnt (RA.SDeclAssign name tpe expr _) = do
  typed <- typeExpr expr
  table <- TA.symTab <$> lift get
  expTpe <- return $ TA.getExprType typed
  if expTpe == tpe
  then return $ TA.SDeclAssign name tpe typed table Void
  else throwE $ "Expression " ++ show expr ++ " is not of type " ++ show tpe

typeStmnt (RA.SBlock block _) = do
  block' <- typeBlock block
  table <- TA.symTab <$> lift get
  return $ TA.SBlock block' table Void
typeStmnt (RA.SWhile expr stmnt _) = do
  expr' <- typeExpr expr
  stmnt' <- typeStmnt stmnt
  table <- TA.symTab <$> lift get
  return $ TA.SWhile expr' stmnt' table Void
typeStmnt (RA.SIf expr stmnt _) = do
  expr' <- typeExpr expr
  stmnt' <- typeStmnt stmnt
  table <- TA.symTab <$> lift get
  return $ TA.SIf expr' stmnt' table Void
typeStmnt (RA.SReturn expr _) = do
  expr' <- typeExpr expr
  table <- TA.symTab <$> lift get
  return $ TA.SReturn expr' table Void

typeExpr :: RA.Expr -> ExceptT String TA.Typer TA.Expr
typeExpr (RA.BOp op exp1 exp2 _) = do
  exp1' <- typeExpr exp1
  exp2' <- typeExpr exp2
  case op of
    Plus -> if (TA.getExprType exp1' /= Int) || (TA.getExprType exp2' /= Int) then
      throwE $ "Type error: Cannot add expressions" ++ show exp1 ++ ", " ++ show exp2
      else return (TA.BOp op exp1' exp2' (TA.getExprTable exp1') Int)
    Minus -> if (TA.getExprType exp1' /= Int) || (TA.getExprType exp2' /= Int) then
      throwE $ "Type error: Cannot subtract expressions" ++ show exp1 ++ ", " ++ show exp2
      else return (TA.BOp op exp1' exp2' (TA.getExprTable exp1') Int)
    Times -> if (TA.getExprType exp1' /= Int) || (TA.getExprType exp2' /= Int) then
      throwE $ "Type error: Cannot multiply expressions" ++ show exp1 ++ ", " ++ show exp2
      else return (TA.BOp op exp1' exp2' (TA.getExprTable exp1') Int)
    Div -> if (TA.getExprType exp1' /= Int) || (TA.getExprType exp2' /= Int) then
      throwE $ "Type error: Cannot divide expressions" ++ show exp1 ++ ", " ++ show exp2
      else return (TA.BOp op exp1' exp2' (TA.getExprTable exp1') Int)
    Lt -> if (TA.getExprType exp1' /= Int) || (TA.getExprType exp2' /= Int) then
      throwE $ "Type error: Cannot compare expressions" ++ show exp1 ++ " to " ++ show exp2
      else return (TA.BOp op exp1' exp2' (TA.getExprTable exp1') Int)
    Lte -> if TA.getExprType exp1' /= Int || (TA.getExprType exp2' /= Int) then
      throwE $ "Type error: Cannot compare expressions" ++ show exp1 ++ " to " ++ show exp2
      else return $ TA.BOp op exp1' exp2' (TA.getExprTable exp1') Int
    Access -> case TA.getExprType exp1' of
      Arr typ _ -> if TA.getExprType exp2' == Int then
        return $ TA.BOp op exp1' exp2' (TA.getExprTable exp1') typ
        else throwE $ "Type error: Cannot access and array with expression " ++ show exp2
      _ -> throwE $ "Type error: cannot access non array " ++ show exp1

typeExpr (RA.EAssign name expr table) = do
  (Entry tpeName _ ) <- maybe (throwE $ "Failed to find '" ++ show name ++ "' in table") return (M.lookup name (vars table))
  expr' <- typeExpr expr
  if TA.getExprType expr' /= tpeName then
      throwE $ "Type error: Cannot assign " ++ show name ++ " to " ++ show expr
      else return (TA.EAssign name expr' (TA.getExprTable expr') tpeName)

typeExpr (RA.UOp op expr _) = do
  expr' <- typeExpr expr
  case op of
    Deref -> case TA.getExprType expr' of
      Pointer tpe -> return $ TA.UOp op expr' (TA.getExprTable expr') tpe
      _ -> throwE $ "Cannot dereference non pointer: " ++ show expr
    Neg -> case TA.getExprType expr' of
      Int -> return $ TA.UOp op expr' (TA.getExprTable expr') Int
      _ -> throwE $ "Cannot negate: " ++ show expr

typeExpr (RA.Lit l)= return $ TA.Lit l
typeExpr (RA.Var v table) = do
  (Entry tpe _ ) <- maybe (throwE $ "Failed to find '" ++ show v ++ "' in table: " ++ show table) return (M.lookup v (vars table))
  return $ TA.Var v emptyTable tpe
typeExpr (RA.Ch c) = return $ TA.Ch c
