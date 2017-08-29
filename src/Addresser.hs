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
import Control.Monad.State.Lazy
import qualified Lib.SymbolTable as ST
import qualified Data.Map as M
import Lib.Types
import Data.Maybe

{-
  If we do this right, our stack should look like
  | fparam n   |  Higher memory
  | fparam n-1 |
  |     ...    |
  | fparam 2   |
  | fparam 1   |
  | old rip    |
  | old rbp    | <- rbp
  | localvar 1 |
  | localvar 2 |
  | localvar 3 | <- rsp
                 Lower memory
-}

data AddressState
  = AddressState {
    symTab :: M.Map Name AA.Address
    , localAddr :: Int
    , argAddr :: Int
  }
  deriving (Eq, Ord, Show)

newtype Addresser a = Addresser { runAddresser :: State AddressState a }
  deriving (Functor, Applicative, Monad, MonadState AddressState)

lookupVar :: Name -> Addresser AA.Address
lookupVar name = do
  s <- get
  return $ fromMaybe
    (error $ "Could not find " ++ toString name ++ " in renamer " ++ show s)
    (M.lookup name (symTab s))

{- Adds a local to the symbol table
   returns the offset given to it
-}
declare :: Name -> Type -> Addresser AA.Address
declare name tpe = do
  s <- get
  let offset = AA.Offset (localAddr s)
  modify $ \s -> s {
    symTab = M.insert name offset (symTab s)
    , localAddr = toSize tpe + localAddr s
    }
  return offset

{- Adds a function arg to the symbol table
   returns the offset given to it
-}
declareArg :: Name -> Type -> Addresser AA.Address
declareArg name tpe = do
  s <- get
  let offset = AA.Offset (localAddr s - toSize tpe)
  modify $ \s -> s {
    symTab = M.insert name offset (symTab s)
    , localAddr = localAddr s - toSize tpe
    }
  return offset

addresser :: TA.Prog -> Either String AA.Prog
addresser = return . addressProg

addressProg :: TA.Prog -> AA.Prog
addressProg (TA.Prog funcs) = let
  globals = foldr (\x y -> case x of
                      (TA.Func typ name types _) -> ST.addGlobal name (FuncDef typ (map fst types)) y)
            ST.emptyTable funcs
  in AA.Prog $ (\x -> evalState (runAddresser(addressFunc x)) (AddressState M.empty 8 (-8))) <$> funcs

addressFunc :: TA.Function -> Addresser AA.Function
addressFunc (TA.Func tpe name tpes stmnts) = do
  _ <- forM tpes (\x -> declareArg (snd x) (fst x))
  stmnts' <- addressStmnts stmnts
  return $ AA.Func tpe name tpes stmnts'

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
addressStmnt (TA.SDecl name tpe1 tpe2) = do
  offset <- declare name tpe1
  return $ AA.SDecl name tpe1 tpe2 offset
addressStmnt (TA.SDeclAssign name tpe1 expr tpe2) = do
  offset <- declare name tpe1
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
addressExpr (TA.Var name tpe) = do
  offset <- lookupVar name
  return $ AA.Var name tpe offset

addressExpr (TA.Ch c) = return $ AA.Ch c
addressExpr (TA.EArr expList tpe) = do
  expList' <- forM expList addressExpr
  return $ AA.EArr expList' tpe
addressExpr (TA.Call name def exprs tpe) = do
  exprs' <- forM exprs addressExpr
  offset <- lookupVar name
  return $ AA.Call name def exprs' tpe offset
