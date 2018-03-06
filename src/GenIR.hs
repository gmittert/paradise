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
genIR :: M.Map ModulePath AA.Prog -> Either String (IRGen [(AA.Function, Stm)])
genIR prog = return $ genProg prog

-- Create IR for a program
genProg :: M.Map ModulePath AA.Prog -> IRGen [(AA.Function, Stm)]
genProg modules = forM (concatMap (\(AA.Prog funcs) -> funcs) (M.elems modules)) genFunc

genFunc :: AA.Function -> IRGen (AA.Function, Stm)
genFunc f@(AA.Func _ name _ _ _ stmnts) = do
  setFunc f
  stmnts <- genStmnts stmnts
  return (f, seqStm [ FPro f
                  , Lab (funcBegin name)
                  , stmnts
                  , Lab (funcEnd name)
                  , FEpi f])
genFunc f@AA.AsmFunc{} = return (f, FPro f)

genStmnts :: AA.Statements -> IRGen Stm
genStmnts (AA.Statements' stmnt _) = genStmnt stmnt
genStmnts (AA.Statements stmnts stmnt _) = do
  stmnts <- genStmnts stmnts
  stmnt <- genStmnt stmnt
  return $ Seq stmnts stmnt

genStmnt :: AA.Statement -> IRGen Stm
genStmnt (AA.SExpr expr _) = Sexp <$> genExpr expr
genStmnt AA.SDecl {} = return $ Sexp (Const 0) -- nop
genStmnt (AA.SDeclArr _ eleType exprs _ addr) = do
  -- Arrays look like (e.g. 2x2)
  -- | dim 2 |                      | arr[11] |
  -- | dim 1 | <-- | dim ptr  |     | arr[10] |
  --               | num dims |     | arr[01] |
  --               | data ptr | --> | arr[00] |
  --                   ^
  --            arr ---|
  let var = case addr of
        -- Global var
        Fixed name -> EName (Label (show name))
        -- Local argument
        Offset i -> Bop Plus FP (Const i)
        -- Function params
        Lib.Types.RegArg {} -> error "Can't redeclare passed arrays"
        Lib.Types.StackArg {} -> error "Can't redeclare passed arrays"
  -- Allocate the array meta data
  let mdata = Move var (Uop Alloc (Const 24))
  -- First, allocate the array data
  let adata = Move (Mem var) (Uop Alloc (Const (toSize eleType * length exprs)))
  -- Set up the number of dimensions (we only do 1 for now)
  let mdims = Move (Bop Plus (Mem var) (Const 8)) (Const 1)
  -- Allocate the dims data
  let ddata = Move (Bop Plus (Mem var) (Const 16)) (Uop Alloc (Const 8))
  -- Set up the dimension array
  let fillDims = Move (Mem (Bop Plus (Mem var) (Const 16))) (Const (length exprs))
  let alloc = mdata `Seq` adata `Seq` mdims `Seq` ddata `Seq` fillDims
  exp <- forM (zip (reverse exprs) [x * toSize eleType | x <- [0,1..]])
    (\(exp, offset) -> Move (Bop Plus (Mem (Mem var)) (Const offset)) <$> genExpr exp)
  return $ seqStm (alloc:exp)
genStmnt (AA.SDeclAssign _ _ expr _ offset)  = do
  let var = case offset of
        Fixed name -> EName (Label (show name))
        Offset i -> Bop Plus FP (Const i)
        Lib.Types.RegArg c s -> Lib.IR.RegArg c s
        Lib.Types.StackArg c s -> Lib.IR.StackArg c s
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
        Arr tpe -> toSize tpe
        _ -> error "Tried to dereference non array"
  e2 <- genExpr exp2
  return $ Mem (Bop Plus (Mem (Mem e1)) (Bop Times e2 (Const size)))
genExpr (AA.BOp op exp1 exp2 _) = do
  e1 <- genExpr exp1
  e2 <- genExpr exp2
  return $ Bop op e1 e2
genExpr (AA.EAssign _ expr _ offset) = do
  let var = case offset of
        Fixed name -> EName (Label (show name))
        Offset i -> Bop Plus FP (Const i)
        Lib.Types.RegArg c s -> Lib.IR.RegArg c s
        Lib.Types.StackArg c s -> Lib.IR.StackArg c s
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
    (Move (Bop Plus (Mem (Mem irArr)) (Bop Times (Const size) irIdx)) irVal)
    (Mem (Bop Plus (Mem (Mem irArr)) (Bop Times (Const size) irIdx)))
-- | A unary operation
genExpr (AA.UOp op exp1 _) = Uop op <$> genExpr exp1
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
    Lib.Types.RegArg c s -> Lib.IR.RegArg c s
    Lib.Types.StackArg c s -> Lib.IR.StackArg c s
genExpr (AA.FuncName qname _) = return $ Mem (EName (Label (show qname)))
genExpr (AA.Ch c) = return $ Const (ord c)
genExpr (AA.Call name _ exprAddrs _) = do
  let exprs = fst <$> exprAddrs
  let addrs = snd <$> exprAddrs
  exprs' <- forM exprs genExpr
  return $ Call (EName (Label (show name))) exprs' addrs

-- | Sequence a list of statements
seqStm :: [Stm] -> Stm
seqStm [] = Sexp (Const 0)
seqStm [x] = x
seqStm (x:xs) = Seq x (seqStm xs)
