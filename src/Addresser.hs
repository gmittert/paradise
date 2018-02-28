{- |
Module      : Addresser
Description : The addresser annotates each name/var in the AST with its
              corresponding offset from the base pointer.
Copyright   : (c) Jason Mittertreiner, 2017
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Addresser where
import qualified Ast.TypedAst as TA
import qualified Ast.AddressedAst as AA
import Control.Monad
import Control.Monad.State.Lazy


import qualified Data.Map as M
import Lib.Types
import Data.Maybe


{-
Following the System V AMD64 ABI,
https://en.wikipedia.org/wiki/X86_calling_conventions#System_V_AMD64_ABI

The first six integer/ptr args are passed in
RDI, RSI, RDX, RCX, R8, R9, and additional args are put on the stack

RBX, RBP and R12-R15 are callee save, everything else is caller save

  If we do this right, our stack should look like
RDI: fparam 1
RSI: fparam 2
RDX: fparam 3
RCX: fparam 4
R8:  fparam 5
R9:  fparam 6
   Higher memory
  | fparam n   |
  | fparam n-1 |
  |     ...    |
  | fparam 8   |
  | fparam 7   |
  | old rip    |
  | old rbp    | <- rbp
  | localvar 1 |
  | localvar 2 |
  | localvar 3 | <- rsp
   Lower memory
-}

data AddressState
  = AddressState {
    symTab :: M.Map Name Address
    , locals :: M.Map Name Address
    , localAddr :: Int
    , argAddr :: Int
  }
  deriving (Eq, Ord, Show)

newtype Addresser a = Addresser { runAddresser :: State AddressState a }
  deriving (Functor, Applicative, Monad, MonadState AddressState)

lookupVar :: Name -> Addresser Address
lookupVar name = do
  s <- get
  return $ fromMaybe
    (error $ "Could not find " ++ toString name ++ " in renamer " ++ show s)
    (M.lookup name (symTab s))

{- Adds a local to the symbol table returns the offset given to it. If the
   type requires more than one byte, we return the lowest byte.
-}
addLocal :: Name -> Type -> Addresser Address
addLocal name tpe = do
  s <- get
  let offset = localAddr s - toSize tpe
  modify $ \s -> s {
    symTab = M.insert name (Offset offset) (symTab s)
    , locals = M.insert name (Offset offset) (locals s)
    , localAddr = offset
    }
  return $ Offset offset

{- Adds a function arg to the symbol table
   returns the offset given to it
-}
addParam :: Name -> Type -> Addresser Address
addParam name _ = do
  s <- get
  let count = argAddr s
  modify $ \s -> s {
    symTab = M.insert name (Arg count) (symTab s)
    , argAddr = count + 1
    }
  return $ Arg count

addresser :: M.Map ModulePath TA.Prog -> Either String (M.Map ModulePath AA.Prog)
addresser prog = forM prog (return . addressProg)

addressProg :: TA.Prog -> AA.Prog
addressProg (TA.Prog funcs) = let
  globals = foldr (\func funcs -> case func of
                      (TA.Func _ qname@(QualifiedName _ name) _ _) -> M.insert name (Fixed qname) funcs)
            M.empty funcs
  in AA.Prog $ (\x -> evalState (runAddresser(addressFunc x)) (AddressState globals M.empty 0 0)) <$> funcs

addressFunc :: TA.Function -> Addresser AA.Function
addressFunc (TA.Func tpe name tpes stmnts) = do
  forM_ tpes (\x -> addParam (snd x) (fst x))
  stmnts' <- addressStmnts stmnts
  s <- get
  return $ AA.Func tpe name tpes (locals s) (localAddr s - 8) stmnts'
addressFunc (TA.AsmFunc tpe name tpes) =
  return $ AA.AsmFunc tpe name tpes

addressStmnts :: TA.Statements -> Addresser AA.Statements
addressStmnts (TA.Statements' stmnt tpe) = do
  stmnt' <- addressStmnt stmnt
  return $ AA.Statements' stmnt' tpe
addressStmnts (TA.Statements stmnts stmnt tpe) = do
  stmnts' <- addressStmnts stmnts
  stmnt' <- addressStmnt stmnt
  return $ AA.Statements stmnts' stmnt' tpe

addressStmnt :: TA.Statement -> Addresser AA.Statement
addressStmnt (TA.SExpr expr tpe) = do
  expr' <- addressExpr expr
  return $ AA.SExpr expr' tpe
addressStmnt (TA.SDecl name tpe1 tpe2) =
  AA.SDecl name tpe1 tpe2 <$> addLocal name tpe1
addressStmnt (TA.SDeclArr name eleTpe exprs arrTpe) = do
  offset <- addLocal name arrTpe
  exprs <- forM exprs addressExpr
  return $ AA.SDeclArr name eleTpe exprs arrTpe offset
addressStmnt (TA.SDeclAssign name tpe1 expr tpe2) = do
  offset <- addLocal name tpe1
  expr' <- addressExpr expr
  return $ AA.SDeclAssign name tpe1 expr' tpe2 offset
addressStmnt (TA.SBlock stmnts tpe) = do
  scope <- get
  stmnts' <- addressStmnts stmnts
  put scope
  return $ AA.SBlock stmnts' tpe
addressStmnt (TA.SWhile expr stmnt tpe) = do
  scope <- get
  expr' <- addressExpr expr
  stmnt' <- addressStmnt stmnt
  put scope
  return $ AA.SWhile expr' stmnt' tpe
addressStmnt (TA.SIf expr stmnt tpe) = do
  scope <- get
  expr' <- addressExpr expr
  stmnt' <- addressStmnt stmnt
  put scope
  return $ AA.SIf expr' stmnt' tpe
addressStmnt (TA.SReturn expr tpe) = do
  scope <- get
  expr' <- addressExpr expr
  put scope
  return $ AA.SReturn expr' tpe

addressExpr :: TA.Expr -> Addresser AA.Expr
addressExpr (TA.BOp op exp1 exp2 tpe) = do
  exp1' <- addressExpr exp1
  exp2' <- addressExpr exp2
  return $ AA.BOp op exp1' exp2' tpe
addressExpr (TA.EAssign name expr tpe) = do
  exp' <- addressExpr expr
  offset <- lookupVar name
  return $ AA.EAssign name exp' tpe offset
addressExpr (TA.EAssignArr e1 e2 e3 tpe) = do
  scope <- get
  e1' <- addressExpr e1
  e2' <- addressExpr e2
  e3' <- addressExpr e3
  put scope
  return $ AA.EAssignArr e1' e2' e3' tpe
addressExpr (TA.UOp op expr tpe) = do
  scope <- get
  exp' <- addressExpr expr
  put scope
  return $ AA.UOp op exp' tpe
addressExpr (TA.Lit l) = return $ AA.Lit l
addressExpr (TA.Var name tpe dir) = do
  offset <- lookupVar name
  return $ AA.Var name tpe offset dir
addressExpr (TA.FuncName name tpe) =
  return $ AA.FuncName name tpe
addressExpr (TA.Ch c) = return $ AA.Ch c
addressExpr (TA.Call name def exprs tpe) = do
  exprs' <- forM exprs addressExpr
  return $ AA.Call name def exprs' tpe
