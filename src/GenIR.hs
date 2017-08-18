module GenIR where

import qualified Ast.TypedAst as TA
import qualified Lib.IR as IR
import Types

data GenIRState = GenIRState {
  instrs :: [IR.Instr]
  , nextTemp :: Int
  , nextLabel :: Int
  , lastAssgn :: IR.Var
}
emptyState :: GenIRState
emptyState = GenIRState [] 0 0 (IR.Var "")

newTemp :: GenIRState -> (IR.Var, GenIRState)
newTemp st = (IR.Var ("$" ++ show (nextTemp st)), st{nextTemp = nextTemp st + 1})

ret :: IR.Var -> GenIRState -> GenIRState
ret v st = st{lastAssgn = v}

newLabel :: GenIRState -> (IR.Label, GenIRState)
newLabel st = (IR.Label ("L" ++ show (nextLabel st)), st{nextLabel = nextLabel st + 1})

addInstrs :: [IR.Instr] -> GenIRState -> GenIRState
addInstrs ists state = state {instrs = instrs state ++ ists}

-- Create the three address code intermediate representation
genIR :: TA.TypedAst -> Either String [IR.Instr]
genIR (TA.TypedAst prog) = return $ instrs $ genProg prog emptyState

genProg :: TA.Prog -> GenIRState -> GenIRState
genProg (TA.Prog blk _ _) = genBlock blk

genBlock :: TA.Block -> GenIRState -> GenIRState
genBlock (TA.Block stmnts _ _) = genStmnts stmnts

genStmnts :: TA.Statements -> GenIRState -> GenIRState
genStmnts (TA.Statements' stmnt _ _) st = genStmnt stmnt st
genStmnts (TA.Statements stmnts stmnt _ _) st =
  let st1 = genStmnts stmnts st
  in genStmnt stmnt st1

genStmnt :: TA.Statement -> GenIRState -> GenIRState
genStmnt (TA.SExpr expr _ _) st = genExpr expr st
genStmnt TA.SDecl {} st = st
genStmnt (TA.SDeclAssign name _ expr _ _) st = let
  st1 = genExpr expr st
  v = lastAssgn st1 in
  addInstrs [IR.Assign (IR.Var (toString name)) (IR.IRVar v)] st1
genStmnt (TA.SBlock block _ _) st = genBlock block st
genStmnt (TA.SWhile expr stmnt _ _) st = let
  (before, st1) = newLabel st
  (end, st2) = newLabel st1
  st3 = addInstrs [IR.Lab before] st2
  st4 = genExpr expr st3
  resvar = lastAssgn st4
  st5 = addInstrs [IR.BrZero resvar end] st4
  st6 = genStmnt stmnt st5
  in
  addInstrs [IR.Lab end] st6
genStmnt (TA.SIf expr stmnt _ _) st = let
  (end, st1) = newLabel st
  st2 = genExpr expr st1
  resvar = lastAssgn st2
  st3 = addInstrs [IR.BrZero resvar end] st2
  st4 = genStmnt stmnt st3
  in
  addInstrs [IR.Lab end] st4

genStmnt (TA.SReturn expr _ _) st = let
  st1 = genExpr expr st
  v = lastAssgn st1
  (t, st2) = newTemp st1
  st3 = addInstrs [IR.Assign t (IR.IRVar v)
                  , IR.Ret t] st2 in
  ret t st3

genExpr :: TA.Expr -> GenIRState -> GenIRState
genExpr (TA.BOp op exp1 exp2 _ _) st = let
  st1 = genExpr exp1 st
  t1 = lastAssgn st1
  st2 = genExpr exp2 st1
  t2 = lastAssgn st2
  (t3, st3) = newTemp st2
  st4 = addInstrs [IR.Assign t3 (IR.IRBOp op t1 t2)] st3 in
  ret t3 st4
genExpr (TA.EAssign name expr _ _) st = let
  st1 = genExpr expr st
  t1 = lastAssgn st1
  res = IR.Var (toString name)
  st2 = addInstrs [IR.Assign res (IR.IRVar t1)] st1 in
  ret res st2
genExpr (TA.UOp op exp1 _ _) st = let
  st1 = genExpr exp1 st
  t1 = lastAssgn st1
  (t3, st2) = newTemp st1
  st3 = addInstrs [IR.Assign t3 (IR.IRUOp op t1)] st2
  in ret t3 st3
genExpr (TA.Lit int) st = let
  (t, st1) = newTemp st
  st2 = addInstrs [IR.Assign t (IR.IRInt int)] st1 in
  ret t st2
genExpr (TA.Var name _ _) st = let
  (t, st1) = newTemp st
  st2 = addInstrs [IR.Assign t (IR.IRVar (IR.Var (toString name)))] st1 in
  ret t st2
genExpr (TA.Ch c) st = let
  (t, st1) = newTemp st
  st2 = addInstrs [IR.Assign t (IR.IRChar c)] st1 in
  ret t st2
