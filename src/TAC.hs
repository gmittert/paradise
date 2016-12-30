{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TAC where

import Syntax
import Semantic
import Types
import Prelude hiding (lookup)
import qualified Data.Map.Strict as M
import Control.Monad.State.Lazy

newtype CodeGen a = CodeGen { genCode :: State CodegenState a }
  deriving (Functor, Applicative, Monad, MonadState CodegenState)


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

data CodegenState
  = CodegenState {
      symtab  :: SymbolTable
    , nextTmp :: Int
    , offset  :: Int
    }
  deriving (Eq, Ord, Show)

lookup :: Name -> CodeGen Entry
lookup name = CodeGen . state $ \s ->
  case M.lookup name (symtab s) of
    Just e -> (e,s)
    Nothing -> error $ "Could not find " ++ toString name
      ++ " in table " ++ show s

insert :: Name -> Type -> CodeGen ()
insert name typ = CodeGen $ state $ \s ->
  let newAddr = offset s + toSize typ in
  ((), CodegenState
       (M.insert name (Entry typ (Addr newAddr)) (symtab s))
       (nextTmp s)
       (offset s + toSize typ))

fresh :: Type -> CodeGen Name
fresh typ = CodeGen $ state $ \s ->
  let newAddr = offset s + toSize typ
      newName = Name $ "@" ++ show (nextTmp s)
  in
  (newName, CodegenState
    (M.insert
       newName
      (Entry typ (Addr newAddr))
      (symtab s))
    (nextTmp s + 1)
    newAddr)

compile :: Prog -> (TacTree, CodegenState)
compile prog = runState (genCode $ genTAC (NProg prog)) (CodegenState M.empty 0 0)

genTAC :: SyntaxNode -> CodeGen TacTree
genTAC (NProg (Prog stmnts ret)) = do
    sTree <- genTAC (NStatements stmnts)
    rAddr <- genTAC (NExpr ret)
    return $ Concat sTree (UInstr Return rAddr)

genTAC (NExpr (Syntax.Var a)) = do
  (Entry _ addr) <- lookup a
  return (IVal addr)
genTAC (NExpr (Lit int)) = return $ IVal (Val int)
genTAC (NExpr (Ch c)) = return $ IVal (Semantic.Char c)
genTAC (NExpr (Op a name expr)) = do
    (Entry _ addr) <- lookup name
    rTree <- genTAC (NExpr expr)
    freshName  <- fresh Int
    return $ BAssign freshName (BInstr a (IVal addr) rTree)
genTAC (NExpr (Boolean b)) = return $ IVal (Semantic.Bool b)
genTAC (NExpr (Syntax.Str s)) = do
  freshName <- fresh (String (length s))
  return $ IStr freshName s

genTAC (NStatements (Statements' stmnt)) = genTAC (NStatement stmnt)
genTAC (NStatements (Statements stmnts stmnt)) = do
  stmnts' <- genTAC (NStatements stmnts)
  stmnt'  <- genTAC (NStatement stmnt)
  return $ Concat stmnts' stmnt'

genTAC (NStatement (SAssign name expr)) = do
    rTree <- genTAC (NExpr expr)
    return $ BAssign name rTree

genTAC (NStatement (SExpr expr)) = genTAC (NExpr expr)
genTAC (NStatement (SPrint expr)) = do
  eTree <- genTAC (NExpr expr)
  return $ UInstr Print eTree

genTAC (NStatement (SDecl name typ)) = do
  insert name typ
  return (IName name)
genTAC (NStatement (SDeclAssign name typ expr)) = do
  insert name typ
  genTAC (NStatement (SAssign name expr))
