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

-- Free all the allocated variables in the current scope
freeScope :: C.GenC [C.Statement]
freeScope = do
  vars <- head . C.alloced <$> get
  modify $ \s -> s { C.alloced = tail (C.alloced s) }
  forM vars (return . C.SExpr . C.Free)

newScope :: C.GenC ()
newScope = modify $ \s -> s { C.alloced = [] : C.alloced s }

alloc :: String -> Int -> C.GenC C.Expr
alloc name size = do
  modify $ \s -> s { C.alloced = (name : head (C.alloced s)) : tail (C.alloced s) }
  return $ C.Malloc (C.Lit size)

genCFunc :: TA.Function -> C.GenC C.Function
genCFunc (TA.Func _ name@(QualifiedName (ModulePath []) (Name "main")) tps stmnts exp) = do
  newScope
  modify $ \s -> s{C.alloced = [] : C.alloced s}
  stmnts' <- forM stmnts genCStm
  exp' <- genCExp exp
  tps' <- forM tps (\(x,y) -> flip (,) y <$> C.toCType x)
  free <- freeScope
  let cfunc = C.Func C.Int name tps' (join stmnts' ++ free) exp'
  modify $ \s -> s {C.defs = S.insert (C.FDeclare cfunc) (C.defs s)}
  return cfunc
genCFunc (TA.Func tpe name tps stmnts exp) = do
  newScope
  stmnts' <- forM stmnts genCStm
  exp' <- genCExp exp
  tpe' <-C.toCType tpe
  tps' <- forM tps (\(x,y) -> flip (,) y <$> C.toCType x)
  free <- freeScope
  let cfunc = C.Func tpe' name tps' (join stmnts' ++ free) exp'
  modify $ \s -> s{C.defs = S.insert (C.FDeclare cfunc)(C.defs s)}
  return cfunc
genCFunc (TA.Proc name tps stmnts) = do
  newScope
  stmnts' <- forM stmnts genCStm
  tps' <- forM tps (\(x,y) -> flip (,) y <$> C.toCType x)
  free <- freeScope
  let cfunc = C.Proc name tps' (join stmnts' ++ free)
  modify $ \s -> s{C.defs = S.insert (C.FDeclare cfunc)(C.defs s)}
  return cfunc

genCFunc (TA.CFunc _ _ _ s) = return $ C.CFunc s

genCStm :: TA.Statement -> C.GenC [C.Statement]
genCStm (TA.SExpr exp _) = do
  exp' <- genCExp exp
  return [C.SExpr exp']
genCStm (TA.SDecl name tpe _) = do
  tpe' <- C.toCType tpe
  return [C.SDecl name tpe']
genCStm (TA.SDeclArr name tpe exprs _) = do
  exprs' <- reverse <$> forM exprs genCExp
  tpe' <- C.toCType tpe
  -- | Alloc the array
  allocArr <- alloc (toString name ++ ".data" ) (length exprs * toSize tpe)
  let create = C.EAssignF (C.Var name) "data" allocArr
  let moveExprs = zipWith (\x y -> C.SExpr $ C.EAssignArr (C.Field (C.Var name) "data") (C.Lit y) x) exprs' [0,1..]
  return $ [ C.SDecl name tpe'
             , C.SExpr create
             , C.SExpr (C.EAssignF (C.Var name) "len" (C.Lit (length exprs)))]
    ++ moveExprs

genCStm (TA.SDeclAssign name tpe expr _) = do
  expr' <- genCExp expr
  tpe' <- C.toCType tpe
  return [C.SDeclAssign name tpe' expr']
genCStm (TA.SBlock stmnts _) = do
  newScope
  stmnts <-  forM stmnts genCStm
  free <- freeScope
  return [C.SBlock (join stmnts ++ free)]
genCStm (TA.SWhile exp stm _) = do
  exp' <- genCExp exp
  stm' <- genCStm stm
  return [C.SWhile exp' (C.SBlock stm')]
genCStm (TA.SIf exp stm _ ) = do
  exp' <- genCExp exp
  stm' <- genCStm stm
  return [C.SIf exp' (C.SBlock stm')]

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
  return $ C.EAssignArr (C.Field e1' "data") e2' e3'
genCExp (TA.UOp op e1 _) = C.UOp op <$> genCExp e1
genCExp (TA.Lit i _ _ ) = return $ C.Lit i
genCExp (TA.Var name _ _) = return $ C.Var name
genCExp (TA.FuncName name _) = return $ C.FuncName name
genCExp (TA.Ch c) = return $ C.Ch c
genCExp (TA.Call name _ args _) = C.Call name <$> forM args genCExp
