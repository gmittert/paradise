{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module GenIR where

import qualified Ast.AddressedAst as AA
import qualified Lib.IR as IR
import Control.Monad.State.Lazy
import Data.Char
import Lib.Types
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
addFunc :: Name -> [(Type, Name)] -> IRGen ()
addFunc name args = modify $ \st -> st
  {instrs = [IR.Func (Name ("func__" ++ toString name)) args] : instrs st}

-- Create the three address code intermediate representation
genIR :: AA.Prog -> Either String [[IR.Instr]]
genIR prog = return $ instrs $ (execState. irgen . genProg) prog emptyState

genProg :: AA.Prog -> IRGen ()
genProg (AA.Prog funcs) = do
  _ <- forM funcs genFunc
  return ()

genFunc :: AA.Function -> IRGen ()
genFunc (AA.Func _ name args stmnts) = do
  addFunc name args
  genStmnts stmnts
  return ()

genStmnts :: AA.Statements -> IRGen ()
genStmnts (AA.Statements' stmnt _) = do
  genStmnt stmnt
  return ()
genStmnts (AA.Statements stmnts stmnt _) = do
  genStmnts stmnts
  genStmnt stmnt
  return ()

genStmnt :: AA.Statement -> IRGen ()
genStmnt (AA.SExpr expr _) = do
  genExpr expr
  return ()
genStmnt AA.SDecl {} = return ()
genStmnt (AA.SDeclAssign name _ expr _ _)  = do
  genExpr expr
  v <- getLastAssgn
  addInstrs [IR.Assign (IR.LVar (IR.Var (toString name) (AA.getExprType expr))) (IR.IRVar v)]
  return ()
genStmnt (AA.SBlock block _) = genStmnts block
genStmnt (AA.SWhile expr stmnt _) = do
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
genStmnt (AA.SIf expr stmnt _) = do
  end <- newLabel
  genExpr expr
  resvar <- getLastAssgn
  addInstrs [IR.BrZero resvar end]
  genStmnt stmnt
  addInstrs [IR.Lab end]
  return ()
genStmnt (AA.SReturn expr _) = do
  genExpr expr
  v <- getLastAssgn
  t <- newTemp (AA.getExprType expr)
  addInstrs [IR.Assign (IR.LVar t) (IR.IRVar v) , IR.Ret t]
  ret t
  return ()

genExpr :: AA.Expr -> IRGen ()
genExpr (AA.BOp op exp1 exp2 tpe) = do
  genExpr exp1
  t1 <- getLastAssgn
  genExpr exp2
  t2 <- getLastAssgn
  t3 <- newTemp tpe
  addInstrs [IR.Assign (IR.LVar t3) (IR.IRBOp op t1 t2)]
  ret t3
  return ()
genExpr (AA.EAssign name expr tpe _) = do
  genExpr expr
  t1 <- getLastAssgn
  let res = IR.Var (toString name) tpe
  addInstrs [IR.Assign (IR.LVar res) (IR.IRVar t1)]
  ret res
  return ()
genExpr (AA.EAssignArr e1 e2 e3 _) = do
  genExpr e1
  t1 <- getLastAssgn
  genExpr e2
  t2 <- getLastAssgn
  genExpr e3
  t3 <- getLastAssgn
  addInstrs [IR.Assign (IR.LAccess t1 t2) (IR.IRVar t3)]
  ret t1
  return ()
genExpr (AA.UOp op exp1 tpe) = do
  genExpr exp1
  t1 <- getLastAssgn
  t3 <- newTemp tpe
  addInstrs [IR.Assign (IR.LVar t3) (IR.IRUOp op t1)]
  ret t3
  return ()
genExpr (AA.Lit int) = do
  t <- newTemp Int
  addInstrs [IR.Assign (IR.LVar t) (IR.RInt int)]
  ret t
  return ()
genExpr (AA.Var name tpe _) = do
  t <- newTemp tpe
  addInstrs [IR.Assign (IR.LVar t) (IR.IRVar (IR.Var (toString name) tpe))]
  ret t
  return ()
genExpr (AA.Ch c) = do
  t <- newTemp Char
  addInstrs [IR.Assign (IR.LVar t) (IR.RInt (ord c))]
  ret t
  return ()
genExpr (AA.EArr exprs tpe) = do
  t <- newTemp tpe
  addInstrs [IR.Assign (IR.LVar t) (IR.IRArr t tpe)]
  _ <- forM (zip exprs [0 ..]) (\expr -> do
                  genExpr (fst expr)
                  res <- getLastAssgn
                  offset <- newTemp tpe
                  addInstrs [IR.Assign (IR.LVar offset) (IR.RInt (snd expr))]
                  addInstrs [IR.Assign (IR.LAccess t offset) (IR.IRVar res)]
                  return ()
                 )
  ret t
  return ()
genExpr (AA.Call name _ exprs tpe _) = do
  temps <- forM (AA.getExprType <$> exprs) newTemp
  _ <- forM (zip exprs temps) (\expr -> do
                  genExpr (fst expr)
                  res <- getLastAssgn
                  addInstrs [IR.Assign (IR.LVar (snd expr)) (IR.IRVar res)]
                  return ()
                 )
  tmp <- newTemp tpe
  addInstrs [IR.Assign (IR.LVar tmp) (IR.Call (IR.Label $ "func__" ++ toString name) temps)]
  ret tmp
  return ()

