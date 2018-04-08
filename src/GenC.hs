module GenC where

import qualified Data.Map.Strict as M

import qualified Ast.CAst as C
import qualified Ast.TypedAst as TA
import Lib.Types

genCProg :: M.Map ModulePath TA.Prog -> Either String C.Prog
genCProg modules = (return . C.Prog) (M.foldr (\(TA.Prog funcs) acc -> map genCFunc funcs ++ acc) [] modules)

genCFunc :: TA.Function -> C.Function
genCFunc (TA.Func tpe name tps stmnts exp) =
  C.Func (C.toCType tpe) name (map (\(x,y) -> (C.toCType x,y)) tps) (genCStms stmnts) (genCExp exp)
genCFunc (TA.Proc name tps stmnts) =
  C.Proc name (map (\(x,y) -> (C.toCType x,y)) tps) (genCStms stmnts)

genCStms :: TA.Statements -> [C.Statement]
genCStms (TA.Statements' stmnt _) = genCStm stmnt
genCStms (TA.Statements stmnts stmnt _) = genCStms stmnts ++ genCStm stmnt

genCStm :: TA.Statement -> [C.Statement]
genCStm (TA.SExpr e _) = [C.SExpr (genCExp e)]
genCStm (TA.SDecl name tpe _) = [C.SDecl name (C.toCType tpe)]
genCStm (TA.SDeclArr name tpe exprs _) = [C.SDeclArr name (C.toCType tpe) (map genCExp exprs)]
genCStm (TA.SDeclAssign name tpe expr _) = [C.SDeclAssign name (C.toCType tpe) (genCExp expr)]
genCStm (TA.SBlock stmnts _) = genCStms stmnts
genCStm (TA.SWhile e s _) = [C.SWhile (genCExp e) (genCStm s)]
genCStm (TA.SIf e s _ ) = [C.SIf (genCExp e) (genCStm s)]

genCExp :: TA.Expr -> C.Expr
genCExp (TA.BOp op e1 e2 _) = C.BOp op (genCExp e1) (genCExp e2)
genCExp (TA.EAssign name exp _) = C.EAssign name (genCExp exp)
genCExp (TA.EAssignArr e1 e2 e3 _) = C.EAssignArr (genCExp e1) (genCExp e2) (genCExp e3)
genCExp (TA.UOp op e1 _) = C.UOp op (genCExp e1)
genCExp (TA.Lit i _ _ ) = C.Lit i
genCExp (TA.Var name _ _) = C.Var name
genCExp (TA.FuncName name _) = C.FuncName name
genCExp (TA.Ch c) = C.Ch c
genCExp (TA.Call name _ args _) = C.Call name (map genCExp args)
