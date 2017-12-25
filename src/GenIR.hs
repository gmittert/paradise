{- |
Module      : GenIR
Description : Generate the intermediate representation
Copyright   : (c) Jason Mittertreiner, 2017
-}
module GenIR where

import qualified Ast.AddressedAst as AA
import Lib.IR
import Data.Char
import Lib.Types
import Control.Monad
import Control.Monad.State.Lazy

-- Create the three address code intermediate representation
genIR :: AA.Prog -> Either String (IRGen [Stm])
genIR prog = return $ genProg prog

genProg :: AA.Prog -> IRGen [Stm]
genProg (AA.Prog funcs) = forM funcs genFunc

genFunc :: AA.Function -> IRGen Stm
genFunc (AA.Func _ name args locals stmnts) = do
  setFunc name
  stmnts <- genStmnts stmnts
  let types = map fst args
  return $ seqStm [ FPro name types locals
                  , Lab (funcBegin name)
                  , stmnts
                  , Lab (funcEnd name)
                  , FEpi name types locals]

genStmnts :: AA.Statements -> IRGen Stm
genStmnts (AA.Statements' stmnt _) = genStmnt stmnt
genStmnts (AA.Statements stmnts stmnt _) = do
  stmnts <- genStmnts stmnts
  stmnt <- genStmnt stmnt
  return $ Seq stmnts stmnt

genStmnt :: AA.Statement -> IRGen Stm
genStmnt (AA.SExpr expr _) = do
  expr <- genExpr expr
  return $ Sexp expr
genStmnt AA.SDecl {} = return $ Sexp (Const 0) -- nop
genStmnt (AA.SDeclArr _ eleType exprs _ offset) = do
  let var = case offset of
        Fixed name -> Mem (EName (Label (show name)))
        Offset i -> Mem (Bop Plus FP (Const i))
        Lib.Types.Arg i -> Lib.IR.Arg i
  exp <- forM (zip exprs (map ((+ 8) . (* toSize eleType)) [0,1..]))
    (\x -> do
        e <- genExpr (fst x)
        return $ Move (Bop Plus var (Const (snd x))) e)
  return $ seqStm exp
genStmnt (AA.SDeclAssign _ _ expr _ offset)  = do
  let var = case offset of
        Fixed name -> Mem (EName (Label (show name)))
        Offset i -> Mem (Bop Plus FP (Const i))
        Lib.Types.Arg i -> Lib.IR.Arg i
  exp <- genExpr expr
  return $ Move var exp
genStmnt (AA.SBlock block _) = genStmnts block
genStmnt (AA.SWhile expr stmnt _) = do
  expr <- genExpr expr
  compL <- newLabel
  topL <- newLabel
  doneL <- newLabel
  stmnt <- genStmnt stmnt
  let comp = Cjump Eq expr (Const 0) topL doneL
  let loop = Jump (JLab compL) [compL]
  return $ seqStm [Lab compL
                  , comp
                  , Lab topL
                  , stmnt
                  , loop
                  , Lab doneL
                  ]
genStmnt (AA.SIf expr stmnt _) = do
  expr <- genExpr expr
  compL <- newLabel
  trueL <- newLabel
  falseL <- newLabel
  stmnt <- genStmnt stmnt
  let comp = Cjump Eq expr (Const 0) trueL falseL
  return $ seqStm [Lab compL
                  , comp
                  , Lab trueL
                  , stmnt
                  , Lab falseL
                  ]
genStmnt (AA.SReturn expr _) = do
  expr <- genExpr expr
  func <- currFunc <$> get
  -- Evaluate the expression then jump to the epilogue
  return $ Seq (Sexp expr) (Jump (JLab (funcEnd func)) [funcEnd func])

genExpr :: AA.Expr -> IRGen Exp
genExpr (AA.BOp Access exp1 exp2 _) = do
  e1 <- genExpr exp1
  let size = case AA.getExprType exp1 of
        Arr tpe _ -> toSize tpe
        _ -> error "Tried to dereference non array"
  e2 <- genExpr exp2
  return $ Bop Plus e1 (Bop Times e2 (Const size))
genExpr (AA.BOp op exp1 exp2 _) = do
  e1 <- genExpr exp1
  e2 <- genExpr exp2
  return $ Bop op e1 e2
genExpr (AA.EAssign _ expr _ offset) = do
  let var = case offset of
        Fixed name -> Mem (EName (Label (show name)))
        Offset i -> Mem (Bop Plus FP (Const i))
        Lib.Types.Arg i -> Lib.IR.Arg i
  exp <- genExpr expr
  return $ Eseq (Move var exp) var

-- | e1[e2] = e3
genExpr (AA.EAssignArr e1 e2 e3 _) = do
  let size = toSize $ AA.getExprType e3
  e1 <- genExpr e1
  e2 <- genExpr e2
  e3 <- genExpr e3
  return $ Eseq
    (Move (Bop Plus e1 (Bop Times (Const size) e2)) e3)
    (Mem (Bop Plus e1 (Bop Times (Const size) e2)))
-- | We don't have unary operations, so we convert them into the equivalent
-- ones: Mem for deref, 0-x for Neg x
genExpr (AA.UOp op exp1 _) =
  case op of
    Deref -> do
      e <- genExpr exp1
      return (Mem e)
    Neg -> do
      e <- genExpr exp1
      let z = Const 0
      return $ Bop Minus z e
    Not -> do
      e <- genExpr exp1
      let c = Const 1
      return $ Bop Minus c e
genExpr (AA.Lit int) = return $ Const int
-- | We get a variable by getting it's offset from the frame pointer then
-- dereferencing it
genExpr (AA.Var _ _ offset) =
  return $ case offset of
    Fixed name -> Mem (EName (Label (show name)))
    Offset i -> Mem (Bop Plus FP (Const i))
    Lib.Types.Arg i -> Lib.IR.Arg i
genExpr (AA.FuncName _ _ offset) =
  return $ case offset of
    Fixed name -> Mem (EName (Label (show name)))
    Offset i -> Mem (Bop Plus FP (Const i))
    Lib.Types.Arg i -> Lib.IR.Arg i
genExpr (AA.Ch c) = return $ Const (ord c)
genExpr (AA.Call name _ exprs _ _) = do
  exprs <- forM exprs genExpr
  return $ Call (EName (Label (toString name))) exprs

-- | Sequence a list of statements
seqStm :: [Stm] -> Stm
seqStm [] = Sexp (Const 0)
seqStm [x] = x
seqStm (x:xs) = Seq x (seqStm xs)
