{-# LANGUAGE LambdaCase #-}
module Typer where
import qualified Ast.ResolvedAst as RA
import qualified Ast.TypedAst as TA
import Types
import qualified Data.Map.Strict as M
import Lib.SymbolTable
import Data.Maybe

typer :: RA.ResolvedAst -> Either String TA.TypedAst
typer (RA.ResolvedAst prog) = fmap TA.TypedAst (typeProg prog)

typeProg :: RA.Prog -> Either String TA.Prog
typeProg (RA.Prog block _) = do
  block'@(TA.Block _ table _) <- typeBlock block
  return $ TA.Prog block' table Int


typeBlock :: RA.Block -> Either String TA.Block
typeBlock (RA.Block stmnts _) = do
  typed <- typeStmnts stmnts
  (table, _) <- case typed of
      TA.Statements' _ table tpe -> Right (table, tpe)
      TA.Statements _ _ table tpe -> Right (table, tpe)
  return $ TA.Block typed table Void

typeStmnts :: RA.Statements -> Either String TA.Statements
typeStmnts (RA.Statements' stmnt _) = do
  typed <- typeStmnt stmnt
  return $ TA.Statements' typed (TA.getStmntTable typed) Void
typeStmnts (RA.Statements _ stmnt _) = do
  typed <- typeStmnt stmnt
  return $ TA.Statements' typed (TA.getStmntTable typed) Void

typeStmnt :: RA.Statement -> Either String TA.Statement
typeStmnt (RA.SExpr expr _) = do
  typed <- typeExpr expr
  return $ TA.SExpr typed (TA.getExprTable typed) Void
typeStmnt (RA.SDecl name tpe _) = Right $ TA.SDecl name tpe undefined Void
typeStmnt (RA.SDeclAssign name tpe expr _) = do
  typed <- typeExpr expr
  table <- return $ TA.getExprTable typed
  expTpe <- return $ TA.getExprType typed
  if expTpe == tpe
  then Right $ TA.SDeclAssign name tpe typed table Void
  else Left $ "Expression " ++ show expr ++ " is not of type " ++ show tpe

typeStmnt (RA.SBlock block _) = do
  block'@(TA.Block _ table' _) <- typeBlock block
  return $ TA.SBlock block' table' Void
typeStmnt (RA.SWhile expr stmnt _) = do
  expr' <- typeExpr expr
  stmnt' <- typeStmnt stmnt
  return $ TA.SWhile expr' stmnt' (TA.getStmntTable stmnt') Void
typeStmnt (RA.SIf expr stmnt _) = do
  expr' <- typeExpr expr
  stmnt' <- typeStmnt stmnt
  return $ TA.SIf expr' stmnt' (TA.getStmntTable stmnt') Void
typeStmnt (RA.SReturn expr _) = do
  expr' <- typeExpr expr
  return $ TA.SReturn expr' (TA.getExprTable expr') Void

typeExpr :: RA.Expr -> Either String TA.Expr
typeExpr (RA.BOp op exp1 exp2 _) = do
  exp1' <- typeExpr exp1
  exp2' <- typeExpr exp2
  case op of
    Plus -> if (TA.getExprType exp1' /= Int) || (TA.getExprType exp2' /= Int) then
      Left $ "Type error: Cannot add expressions" ++ show exp1 ++ ", " ++ show exp2
      else Right (TA.BOp op exp1' exp2' (TA.getExprTable exp1') Int)
    Minus -> if (TA.getExprType exp1' /= Int) || (TA.getExprType exp2' /= Int) then
      Left $ "Type error: Cannot subtract expressions" ++ show exp1 ++ ", " ++ show exp2
      else Right (TA.BOp op exp1' exp2' (TA.getExprTable exp1') Int)
    Times -> if (TA.getExprType exp1' /= Int) || (TA.getExprType exp2' /= Int) then
      Left $ "Type error: Cannot multiply expressions" ++ show exp1 ++ ", " ++ show exp2
      else Right (TA.BOp op exp1' exp2' (TA.getExprTable exp1') Int)
    Div -> if (TA.getExprType exp1' /= Int) || (TA.getExprType exp2' /= Int) then
      Left $ "Type error: Cannot divide expressions" ++ show exp1 ++ ", " ++ show exp2
      else Right (TA.BOp op exp1' exp2' (TA.getExprTable exp1') Int)
    Lt -> if (TA.getExprType exp1' /= Int) || (TA.getExprType exp2' /= Int) then
      Left $ "Type error: Cannot compare expressions" ++ show exp1 ++ " to " ++ show exp2
      else Right (TA.BOp op exp1' exp2' (TA.getExprTable exp1') Int)
    Lte -> if TA.getExprType exp1' /= Int || (TA.getExprType exp2' /= Int) then
      Left $ "Type error: Cannot compare expressions" ++ show exp1 ++ " to " ++ show exp2
      else Right $ TA.BOp op exp1' exp2' (TA.getExprTable exp1') Int
    Access -> case TA.getExprType exp1' of
      Arr typ _ -> if TA.getExprType exp2' == Int then
        Right $ TA.BOp op exp1' exp2' (TA.getExprTable exp1') typ
        else Left $ "Type error: Cannot access and array with expression " ++ show exp2
      _ -> Left $ "Type error: cannot access non array " ++ show exp1

typeExpr (RA.EAssign name expr table) = do
  (Entry tpeName _ ) <- return $ fromJust $ M.lookup name (vars table)
  expr' <- typeExpr expr
  if TA.getExprType expr' /= tpeName then
      Left $ "Type error: Cannot assign " ++ show name ++ " to " ++ show expr
      else Right (TA.EAssign name expr' (TA.getExprTable expr') tpeName)

typeExpr (RA.UOp op expr _) = do
  expr' <- typeExpr expr
  case op of
    Deref -> case TA.getExprType expr' of
      Pointer tpe -> Right $ TA.UOp op expr' (TA.getExprTable expr') tpe
      _ -> Left $ "Cannot dereference non pointer: " ++ show expr
    Neg -> case TA.getExprType expr' of
      Int -> Right $ TA.UOp op expr' (TA.getExprTable expr') Int
      _ -> Left $ "Cannot negate: " ++ show expr

typeExpr (RA.Lit l)= return $ TA.Lit l
typeExpr (RA.Var v table) = let
  (Entry tpe _ ) = fromJust $ M.lookup v (vars table)
  in return $ TA.Var v emptyTable tpe
typeExpr (RA.Ch c) = return $ TA.Ch c
