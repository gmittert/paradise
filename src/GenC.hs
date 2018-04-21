{- |
Module      : C.GenC
Description : Generate the C code for the program
Copyright   : (c) Jason Mittertreiner, 2017
-}
module GenC where

import qualified Data.Map.Strict as M
import Control.Monad
import Control.Monad.State.Lazy

import qualified Data.Set as S
import qualified Ast.CAst as C
import qualified Ast.TypedAst as TA
import Lib.Types

runCGen :: M.Map ModulePath TA.Prog -> Either String C.Prog
runCGen modules = let (funcs, decls) = runState (C.genC (genCProg modules)) C.emptyState in return $ C.Prog (S.toList (C.defs decls)) funcs
genCProg :: M.Map ModulePath TA.Prog -> C.GenC [C.Function]
genCProg modules = let progs = M.foldr (:) [] modules in
    join <$> forM progs (\(TA.Prog funcs) -> forM funcs genCFunc)

genCFunc :: TA.Function -> C.GenC C.Function
genCFunc (TA.Func _ name@(QualifiedName (ModulePath []) (Name "main")) tps stmnts exp) = do
  stmnts' <- forM stmnts genCStm
  exp' <- genCExp exp
  tps' <- forM tps (\(x,y) -> flip (,) y <$> C.toCType x)
  let cfunc = C.Func C.Int name tps' stmnts' exp'
  modify $ \s -> s{C.defs = S.insert (C.FDeclare cfunc) (C.defs s)} 
  return cfunc
genCFunc (TA.Func tpe name tps stmnts exp) = do
  stmnts' <- forM stmnts genCStm
  exp' <- genCExp exp
  tpe' <-C.toCType tpe
  tps' <- forM tps (\(x,y) -> flip (,) y <$> C.toCType x)
  let cfunc = C.Func tpe' name tps' stmnts' exp'
  modify $ \s -> s{C.defs = S.insert (C.FDeclare cfunc)(C.defs s)}
  return cfunc
genCFunc (TA.Proc name tps stmnts) = do
  stmnts' <- forM stmnts genCStm
  tps' <- forM tps (\(x,y) -> flip (,) y <$> C.toCType x)
  let cfunc = C.Proc name tps' stmnts'
  modify $ \s -> s{C.defs = S.insert (C.FDeclare cfunc)(C.defs s)}
  return cfunc

genCFunc (TA.CFunc _ _ _ s) = return $ C.CFunc s

genCStm :: TA.Statement -> C.GenC C.Statement
genCStm (TA.SExpr exp _) = C.SExpr <$> genCExp exp
genCStm (TA.SDecl name tpe _) = C.SDecl name <$> C.toCType tpe
genCStm (TA.SDeclArr name tpe exprs _) = do
  exprs' <- forM exprs genCExp
  tpe' <- C.toCType tpe
  return $ C.SDeclArr name tpe' exprs'
genCStm (TA.SDeclAssign name tpe expr _) = do
  expr' <- genCExp expr
  tpe' <- C.toCType tpe
  return $ C.SDeclAssign name tpe' expr'
genCStm (TA.SBlock stmnts _) = C.SBlock <$> forM stmnts genCStm
genCStm (TA.SWhile exp stm _) = do
  exp' <- genCExp exp
  stm' <- genCStm stm
  return $ C.SWhile exp' stm'
genCStm (TA.SIf exp stm _ ) = do
  exp' <- genCExp exp
  stm' <- genCStm stm
  return $ C.SIf exp' stm'

genCExp :: TA.Expr -> C.GenC C.Expr
genCExp (TA.BOp op e1 e2 _) = do
  e1' <- genCExp e1
  e2' <-genCExp e2
  return $ C.BOp op e1' e2'
genCExp (TA.EAssign name exp _) = C.EAssign name <$> genCExp exp
genCExp (TA.EAssignArr e1 e2 e3 _) = do
  e1' <- genCExp e1
  e2' <- genCExp e2
  e3' <- genCExp e3
  return $ C.EAssignArr e1' e2' e3'
genCExp (TA.UOp op e1 _) = C.UOp op <$> genCExp e1
genCExp (TA.Lit i _ _ ) = return $ C.Lit i
genCExp (TA.Var name _ _) = return $ C.Var name
genCExp (TA.FuncName name _) = return $ C.FuncName name
genCExp (TA.Ch c) = return $ C.Ch c
genCExp (TA.Call name _ args _) = C.Call name <$> forM args genCExp
