module TAC where

import Syntax
import Semantic
import Types
import Prelude hiding (lookup)
import qualified Data.Map.Strict as M
import Control.Monad.State.Lazy

data UniOp = Neg | Print | Return
  deriving (Eq, Ord, Show)

data TacTree
  = BInstr BinOp TacTree TacTree
  | BAssign Name TacTree
  | Concat TacTree TacTree
  | UInstr UniOp TacTree
  | IVal Addr
  | IName Name
  | IStr Name String
  | Call Int TacTree
  deriving (Eq, Ord, Show)

lookup :: Name -> CodeGen Entry
lookup name = CodeGen . state $ \s ->
  case M.lookup name ((vars.symTab) s) of
    Just e -> (e,s)
    Nothing -> error $ "Could not find " ++ toString name
      ++ " in table " ++ show s

insert :: Name -> Type -> CodeGen ()
insert name typ =
  modify $ \s -> let newAddr = offset s + toSize typ in s{
    symTab = addVar
             name
             (Entry typ (Addr newAddr))
             (symTab s)
    , nextTmp = nextTmp s + 1
    , offset = newAddr
    }

fresh :: Type -> CodeGen Name
fresh typ = CodeGen $ state $ \s ->
  let newAddr = offset s + toSize typ
      newName = Name $ "@" ++ show (nextTmp s)
  in
  (newName, s{
      symTab = addVar
        newName
        (Entry typ (Addr newAddr))
        (symTab s)
    , nextTmp = nextTmp s + 1
    , offset = newAddr})

compile :: Prog -> (TacTree, CodegenState)
compile prog = runState (genCode $ genProg prog)
  (CodegenState (SymbolTable M.empty M.empty) 0 0)

genProg :: Prog -> CodeGen TacTree
genProg (Prog block _) = genBlock block


genBlock :: Block -> CodeGen TacTree
genBlock (Block stmnts scope) = do
  parentState <- get
  put scope
  stmnts' <- genStmnts stmnts
  put parentState
  return stmnts'

genExpr :: Expr -> CodeGen TacTree
genExpr (Var a scope) = do
  parentScope <- get
  put scope
  (Entry _ addr) <- lookup a
  put parentScope
  return (IVal addr)
genExpr (Lit int _) = return $ IVal (IInt int)
genExpr (Ch c _) = return $ IVal (IChar c)
genExpr (Op a name expr scope) = do
  parentScope <- get
  put scope
  (Entry _ addr) <- lookup name
  rTree <- genExpr expr
  freshName  <- fresh Int
  put parentScope
  return $ BAssign freshName (BInstr a (IVal addr) rTree)
genExpr (Boolean b _) = return $ IVal (IBool b)
genExpr (Str s _) = do
  freshName <- fresh (String (length s))
  return $ IStr freshName s

genStmnts :: Statements -> CodeGen TacTree
genStmnts (Statements' stmnt _) = genStmnt stmnt
genStmnts (Statements stmnts stmnt scope) = do
  parentScope <- get
  put scope
  stmnts' <- genStmnts stmnts
  stmnt'  <- genStmnt stmnt
  put parentScope
  return $ Concat stmnts' stmnt'
genStmnt :: Statement -> CodeGen TacTree
genStmnt (SAssign name expr scope) = do
  parentScope <- get
  put scope
  rTree <- genExpr expr
  put parentScope
  return $ BAssign name rTree
genStmnt (SExpr expr _) = genExpr expr
genStmnt (SPrint expr scope) = do
  parentScope <- get
  put scope
  eTree <- genExpr expr
  put parentScope
  return $ UInstr Print eTree
genStmnt (SDecl name typ _) = do
  insert name typ
  return (IName name)
genStmnt (SDeclAssign name typ expr scope) = do
  assign <- genStmnt (SAssign name expr scope)
  insert name typ
  return assign
genStmnt (SBlock vblock table) = genBlock vblock
