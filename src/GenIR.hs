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
genStmnt AA.SDecl {} = return $ Sexp (int 0) -- nop
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
        Fixed name -> EName (Label (show name)) 8
        -- Local argument
        Offset i -> Bop Plus FP (int i) 8
        -- Function params
        Lib.Types.RegArg {} -> error "Can't redeclare passed arrays"
        Lib.Types.StackArg {} -> error "Can't redeclare passed arrays"
  -- Allocate the array meta data
  let mdata = Move var (Uop Alloc (int 24) 8)
  -- First, allocate the array data
  let adata = Move (Mem var 8) (Uop Alloc (int (Lib.Types.toSize eleType * length exprs)) 8)
  -- Set up the number of dimensions (we only do 1 for now)
  let mdims = Move (Bop Plus (Mem var 8) (int 8) 8) (int 1)
  -- Allocate the dims data
  let ddata = Move (Bop Plus (Mem var 8) (int 16) 8) (Uop Alloc (int 8) 8)
  -- Set up the dimension array
  let fillDims = Move (Mem (Bop Plus (Mem var 8) (int 16) 8) 8) (int (length exprs))
  let alloc = Seq mdata (Seq adata (Seq mdims (Seq ddata fillDims)))
  exp <- forM (zip (reverse exprs) [x * Lib.Types.toSize eleType | x <- [0,1..]])
    (\(exp, offset) -> Move (Bop Plus (Mem (Mem var 8) 8) (int offset) 8) <$> genExpr exp)
  return $ seqStm (alloc:exp)
genStmnt (AA.SDeclAssign _ _ expr _ offset)  = do
  let var = case offset of
        Fixed name -> EName (Label (show name)) 8
        Offset i -> Bop Plus FP (int i) 8
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
  let comp = Cjump Eq expr (int 0) doneL topL
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
  let comp = Cjump Eq expr (int 0) falseL trueL
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
genExpr (AA.BOp Access exp1 exp2 tpe) = do
  let sz = Lib.Types.toSize tpe
  e1 <- genExpr exp1
  e2 <- genExpr exp2
  --return $ Mem (Bop Plus (Mem (Mem e1 8) 8) (Bop Times e2 (int size) 8) 8) sz
  return $ Bop Access (Mem (Mem e1 8) 8) e2 sz
genExpr (AA.BOp op exp1 exp2 tpe) = do
  let sz = Lib.Types.toSize tpe
  e1 <- genExpr exp1
  e2 <- genExpr exp2
  return $ Bop op e1 e2 sz
genExpr (AA.EAssign _ expr tpe offset) = do
  let size = Lib.Types.toSize tpe
  let var = case offset of
        Fixed name -> EName (Label (show name)) 8
        Offset i -> Bop Plus FP (int i) 8
        Lib.Types.RegArg c s -> Lib.IR.RegArg c s
        Lib.Types.StackArg c s -> Lib.IR.StackArg c s
  exp <- genExpr expr
  return $ Eseq (Move var exp) var size

-- | Generate an array assignment
-- arr[idx] = val
genExpr (AA.EAssignArr arr idx val _) = do
  let size = Lib.Types.toSize $ AA.getExprType val
  irArr <- genExpr arr
  irIdx <- genExpr idx
  irVal <- genExpr val
  return $ Eseq
    (Move (Bop Plus (Mem (Mem irArr 8) 8) (Bop Times (int size) irIdx 8) 8) irVal)
    (Mem (Bop Plus (Mem (Mem irArr 8) 8) (Bop Times (int size) irIdx 8) 8) 8) size
-- | A unary operation
genExpr (AA.UOp op exp1 tpe) = do
  exp1' <- genExpr exp1
  return $ Uop op exp1' (Lib.Types.toSize tpe)
genExpr (AA.Lit i sz s) = return $ Const i (Lib.Types.toSize (Int sz s))
-- | We get a variable by getting it's offset from the frame pointer then
-- dereferencing it
genExpr (AA.Var _ tpe offset dir) =
  let sz = Lib.Types.toSize tpe in
  return $ case offset of
    Fixed name -> case dir of
      LVal -> EName (Label (show name)) 8
      RVal -> Mem (EName (Label (show name)) 8) sz
    Offset i -> case dir of
      LVal -> Bop Plus FP (int i) 8
      RVal -> Mem (Bop Plus FP (int i) 8) sz
    Lib.Types.RegArg c s -> Lib.IR.RegArg c s
    Lib.Types.StackArg c s -> Lib.IR.StackArg c s
genExpr (AA.FuncName qname _) = return $ Mem (EName (Label (show qname))8) 8
genExpr (AA.Ch c) = return $ int (ord c)
genExpr (AA.Call name _ exprAddrs tpe) = do
  let sz = Lib.Types.toSize tpe
  let exprs = fst <$> exprAddrs
  let addrs = snd <$> exprAddrs
  exprs' <- forM exprs genExpr
  return $ Call (EName (Label (show name)) 8) exprs' addrs sz

-- | Sequence a list of statements
seqStm :: [Stm] -> Stm
seqStm [] = Sexp (int 0)
seqStm [x] = x
seqStm (x:xs) = Seq x (seqStm xs)
