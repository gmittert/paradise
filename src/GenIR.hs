{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module GenIR where

import qualified Ast.TypedAst as TA
import qualified Lib.IR as IR
import Control.Monad.State.Lazy
import Data.Char
import Types
import qualified Data.Map as M

data GenIRState = GenIRState {
  instrs :: [[IR.Instr]]
  , nextTemp :: Int
  , nextLabel :: Int
  , lastAssgn :: IR.Var
  , currOffset :: Int
  , offsets :: M.Map Name Int
}

emptyState :: GenIRState
emptyState = GenIRState [] 0 0 (IR.Var "" Int) 0 M.empty

newtype IRGen a = IRGen { irgen :: State GenIRState a }
  deriving (Functor, Applicative, Monad, MonadState GenIRState)

newTemp :: Type -> IRGen IR.Var
newTemp tpe = do
  st <- get
  modify $ \st -> st{
    nextTemp = nextTemp st + 1
    , currOffset = currOffset st + toSize tpe
    }
  return $ IR.Var ("$" ++ show (nextTemp st)) tpe

ret :: IR.Var -> IRGen ()
ret v = do
  modify $ \st -> st{lastAssgn = v}
  return ()

getLastAssgn :: IRGen IR.Var
getLastAssgn = do
  st <- get
  return $ lastAssgn st

newLabel :: IRGen IR.Label
newLabel = do
  modify $ \st -> st{nextLabel = nextLabel st + 1}
  st <- get
  return $ IR.Label $ "L" ++ show (nextLabel st)

addInstrs :: [IR.Instr] -> IRGen ()
addInstrs ists = modify $ \st -> st {instrs = let (x:xs) = instrs st in
                                        (x ++ ists) : xs}
addFunc :: Name -> IRGen ()
addFunc name = modify $ \st -> st
  {instrs = [IR.Func (Name ("func__" ++ toString name))] : instrs st}

-- Create the three address code intermediate representation
genIR :: TA.Prog -> Either String [[IR.Instr]]
genIR prog = return $ instrs $ (execState. irgen . genProg) prog emptyState

genProg :: TA.Prog -> IRGen ()
genProg (TA.Prog funcs) = do
  _ <- forM funcs genFunc
  return ()

genFunc :: TA.Function -> IRGen ()
genFunc (TA.Func _ name _ stmnts) = do
  addFunc name
  genStmnts stmnts
  return ()

genStmnts :: TA.Statements -> IRGen ()
genStmnts (TA.Statements' stmnt _) = do
  genStmnt stmnt
  return ()
genStmnts (TA.Statements stmnts stmnt _) = do
  genStmnts stmnts
  genStmnt stmnt
  return ()

genStmnt :: TA.Statement -> IRGen ()
genStmnt (TA.SExpr expr _) = do
  genExpr expr
  return ()
genStmnt TA.SDecl {} = return ()
genStmnt (TA.SDeclAssign name _ expr _)  = do
  genExpr expr
  v <- getLastAssgn
  addInstrs [IR.Assign (IR.RVar (IR.Var (toString name) (TA.getExprType expr))) (IR.IRVar v)]
  return ()
genStmnt (TA.SBlock block _) = genStmnts block
genStmnt (TA.SWhile expr stmnt _) = do
  before <- newLabel
  end <- newLabel
  addInstrs [IR.Lab before]
  genExpr expr
  resvar <- getLastAssgn
  addInstrs [IR.BrZero resvar end]
  genStmnt stmnt
  addInstrs [IR.Goto before]
  addInstrs [IR.Lab end]
  return ()
genStmnt (TA.SIf expr stmnt _) = do
  end <- newLabel
  genExpr expr
  resvar <- getLastAssgn
  addInstrs [IR.BrZero resvar end]
  genStmnt stmnt
  addInstrs [IR.Lab end]
  return ()
genStmnt (TA.SReturn expr _) = do
  genExpr expr
  v <- getLastAssgn
  t <- newTemp (TA.getExprType expr)
  addInstrs [IR.Assign (IR.RVar t) (IR.IRVar v) , IR.Ret t]
  ret t
  return ()

genExpr :: TA.Expr -> IRGen ()
genExpr (TA.BOp op exp1 exp2 tpe) = do
  genExpr exp1
  t1 <- getLastAssgn
  genExpr exp2
  t2 <- getLastAssgn
  t3 <- newTemp tpe
  addInstrs [IR.Assign (IR.RVar t3) (IR.IRBOp op t1 t2)]
  ret t3
  return ()
genExpr (TA.EAssign name expr tpe) = do
  genExpr expr
  t1 <- getLastAssgn
  let res = IR.Var (toString name) tpe
  addInstrs [IR.Assign (IR.RVar res) (IR.IRVar t1)]
  ret res
  return ()
genExpr (TA.EAssignArr e1 e2 e3 _) = do
  genExpr e1
  t1 <- getLastAssgn
  genExpr e2
  t2 <- getLastAssgn
  genExpr e3
  t3 <- getLastAssgn
  addInstrs [IR.Assign (IR.RAccess t1 t2) (IR.IRVar t3)]
  ret t1
  return ()
genExpr (TA.UOp op exp1 tpe) = do
  genExpr exp1
  t1 <- getLastAssgn
  t3 <- newTemp tpe
  addInstrs [IR.Assign (IR.RVar t3) (IR.IRUOp op t1)]
  ret t3
  return ()
genExpr (TA.Lit int) = do
  t <- newTemp Int
  addInstrs [IR.Assign (IR.RVar t) (IR.LInt int)]
  ret t
  return ()
genExpr (TA.Var name tpe) = do
  t <- newTemp tpe
  addInstrs [IR.Assign (IR.RVar t) (IR.IRVar (IR.Var (toString name) tpe))]
  ret t
  return ()
genExpr (TA.Ch c) = do
  t <- newTemp Char
  addInstrs [IR.Assign (IR.RVar t) (IR.LInt (ord c))]
  ret t
  return ()
genExpr (TA.EArr exprs tpe) = do
  t <- newTemp tpe
  addInstrs [IR.Assign (IR.RVar t) (IR.IRArr t tpe)]
  _ <- forM (zip exprs [0 ..]) (\expr -> do
                  genExpr (fst expr)
                  res <- getLastAssgn
                  offset <- newTemp tpe
                  addInstrs [IR.Assign (IR.RVar offset) (IR.LInt (snd expr))]
                  addInstrs [IR.Assign (IR.RAccess t offset) (IR.IRVar res)]
                  return ()
                 )
  ret t
  return ()
