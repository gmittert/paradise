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
import qualified Data.Map as M

-- Create the intermediate representation
genIR :: M.Map ModulePath AA.Prog -> Either String (IRGen [Stm])
genIR prog = return $ genProg prog

-- Create IR for a program
genProg :: M.Map ModulePath AA.Prog -> IRGen [Stm]
genProg modules = forM (concatMap (\(AA.Prog funcs) -> funcs) (M.elems modules)) genFunc

genFunc :: AA.Function -> IRGen Stm
genFunc f@(AA.Func _ name _ _ _ stmnts) = do
  setFunc f
  stmnts <- genStmnts stmnts
  return $ seqStm [ FPro f
                  , Lab (funcBegin name)
                  , stmnts
                  , Lab (funcEnd name)
                  , FEpi f]
genFunc AA.AsmFunc{} = return $ seqStm []

genStmnts :: AA.Statements -> IRGen Stm
genStmnts (AA.Statements' stmnt _) = genStmnt stmnt
genStmnts (AA.Statements stmnts stmnt _) = do
  stmnts <- genStmnts stmnts
  stmnt <- genStmnt stmnt
  return $ Seq stmnts stmnt

genStmnt :: AA.Statement -> IRGen Stm
genStmnt (AA.SExpr expr _) = Sexp <$> genExpr expr
genStmnt AA.SDecl {} = return $ Sexp (Const 0) -- nop
genStmnt (AA.SDeclArr _ eleType exprs _ offset) = do
  let var = case offset of
        Fixed name -> EName (Label (show name))
        Offset i -> Bop Plus FP (Const i)
        Lib.Types.Arg i -> Lib.IR.Arg i
  exp <- forM (zip (reverse exprs) [x * toSize eleType | x <- [0,1..]])
    (\x -> Move (Bop Plus var (Const (snd x))) <$> genExpr (fst x))
  return $ seqStm exp
genStmnt (AA.SDeclAssign _ _ expr _ offset)  = do
  let var = case offset of
        Fixed name -> EName (Label (show name))
        Offset i -> Bop Plus FP (Const i)
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
  let comp = Cjump Eq expr (Const 0) doneL topL
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
  let comp = Cjump Eq expr (Const 0) falseL trueL
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
  return $ Seq (Ret expr) (Jump (JLab (funcEnd func)) [funcEnd func])

genExpr :: AA.Expr -> IRGen Exp
genExpr (AA.BOp Access exp1 exp2 _) = do
  e1 <- genExpr exp1
  let size = case AA.getExprType exp1 of
        Arr tpe _ -> toSize tpe
        _ -> error "Tried to dereference non array"
  e2 <- genExpr exp2
  return $ Mem (Bop Plus e1 (Bop Times e2 (Const size)))
genExpr (AA.BOp op exp1 exp2 _) = do
  e1 <- genExpr exp1
  e2 <- genExpr exp2
  return $ Bop op e1 e2
genExpr (AA.EAssign _ expr _ offset) = do
  let var = case offset of
        Fixed name -> EName (Label (show name))
        Offset i -> Bop Plus FP (Const i)
        Lib.Types.Arg i -> Lib.IR.Arg i
  exp <- genExpr expr
  return $ Eseq (Move var exp) var

-- | Generate an array assignment
-- arr[idx] = val
genExpr (AA.EAssignArr arr idx val _) = do
  let size = toSize $ AA.getExprType val
  irArr <- genExpr arr
  irIdx <- genExpr idx
  irVal <- genExpr val
  return $ Eseq
    (Move (Bop Plus irArr (Bop Times (Const size) irIdx)) irVal)
    (Mem (Bop Plus irArr (Bop Times (Const size) irIdx)))
-- | We don't have unary operations, so we convert them into the equivalent
-- ones: Mem for deref, 0-x for Neg x
genExpr (AA.UOp op exp1 _) =
  case op of
    Deref -> Mem <$> genExpr exp1
    Neg -> Bop Minus (Const 0) <$> genExpr exp1
    Not -> Bop Minus (Const 1) <$> genExpr exp1
genExpr (AA.Lit int) = return $ Const int
-- | We get a variable by getting it's offset from the frame pointer then
-- dereferencing it
genExpr (AA.Var _ _ offset dir) =
  return $ case offset of
    Fixed name -> case dir of
      LVal -> EName (Label (show name))
      RVal -> Mem (EName (Label (show name)))
    Offset i -> case dir of
      LVal -> Bop Plus FP (Const i)
      RVal -> Mem (Bop Plus FP (Const i))
    Lib.Types.Arg i -> Lib.IR.Arg i
genExpr (AA.FuncName qname _) = return $ Mem (EName (Label (show qname)))
genExpr (AA.Ch c) = return $ Const (ord c)
genExpr (AA.Call name _ exprs _) =
  Call (EName (Label (show name))) <$> forM exprs genExpr

-- | Sequence a list of statements
seqStm :: [Stm] -> Stm
seqStm [] = Sexp (Const 0)
seqStm [x] = x
seqStm (x:xs) = Seq x (seqStm xs)
