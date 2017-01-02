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
  deriving (Eq, Ord, Show)

lookup :: Name -> CodeGen Entry
lookup name = CodeGen . state $ \s ->
  case M.lookup name ((vars.symTab) s) of
    Just e -> (e,s)
    Nothing -> error $ "Could not find " ++ toString name
      ++ " in table " ++ show s

insert :: Name -> Type -> CodeGen ()
insert name typ = do
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
genProg (Prog block tab) = genRetBlock block

genRetBlock :: RetBlock -> CodeGen TacTree
genRetBlock = undefined

genVoidBlock :: VoidBlock -> CodeGen TacTree
genVoidBlock = undefined

genExpr :: Expr -> CodeGen TacTree
genExpr (Var a table) = do
  (Entry _ addr) <- lookup a
  return (IVal addr)
genExpr (Lit int table) = return $ IVal (IInt int)
genExpr (Ch c table) = return $ IVal (IChar c)
genExpr (Op a name expr table) = do
    (Entry _ addr) <- lookup name
    rTree <- genExpr expr
    freshName  <- fresh Int
    return $ BAssign freshName (BInstr a (IVal addr) rTree)
genExpr (Boolean b table) = return $ IVal (IBool b)
genExpr (Str s table) = do
  freshName <- fresh (String (length s))
  return $ IStr freshName s
genExpr (EBlock retBlock table) = genRetBlock retBlock

genStmnts :: Statements -> CodeGen TacTree
genStmnts (Statements' stmnt table) = genStmnt stmnt
genStmnts (Statements stmnts stmnt table) = do
  stmnts' <- genStmnts stmnts
  stmnt'  <- genStmnt stmnt
  return $ Concat stmnts' stmnt'

genStmnt :: Statement -> CodeGen TacTree
genStmnt (SAssign name expr table) = do
    rTree <- genExpr expr
    return $ BAssign name rTree
genStmnt (SExpr expr table) = genExpr expr
genStmnt (SPrint expr table) = do
  eTree <- genExpr expr
  return $ UInstr Print eTree
genStmnt (SDecl name typ table) = do
  insert name typ
  return (IName name)
genStmnt (SDeclAssign name typ expr table) = do
  insert name typ
  genStmnt (SAssign name expr table)
genStmnt (SBlock vblock table) = genVoidBlock vblock
