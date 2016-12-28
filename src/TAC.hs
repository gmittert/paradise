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
  | BAssign Int TacTree
  | Concat TacTree TacTree
  | UInstr UniOp TacTree
  | IAddr Addr
  deriving (Eq, Ord, Show)

data CodegenState
  = CodegenState {
      symtab :: SymbolTable
    , nextTmp :: Int
    , offset  :: Int
    }
  deriving (Eq, Ord, Show)

lookup :: Name -> CodeGen TacTree
lookup name = CodeGen . state $ \s ->
  case M.lookup name (symtab s) of
    Just (Entry _ addr) -> (IAddr addr, s)
    Nothing -> error $ "Could not find " ++ toString name
      ++ " in table " ++ show s

insert :: Name -> Type -> CodeGen TacTree
insert name typ = CodeGen $ state $ \s ->
  (IAddr (Addr (offset s)), CodegenState
                    (M.insert name (Entry typ (Addr (offset s))) (symtab s))
                    (nextTmp s)
                    (offset s + toSize typ))

fresh :: Type -> CodeGen TacTree
fresh typ = CodeGen $ state $ \s ->
  (IAddr (Addr (offset s)), CodegenState
    (M.insert
      (Name $ "@" ++ show (nextTmp s))
      (Entry typ (Addr (offset s)))
      (symtab s))
    (nextTmp s + 1)
    (offset s + toSize typ))

compile :: Prog -> TacTree
compile prog = evalState (genCode $ genTAC (NProg prog)) (CodegenState M.empty 0 0)

genTAC :: SyntaxNode -> CodeGen TacTree
genTAC (NProg (Prog decls stmnts ret)) = do
    _ <- genTAC (NDecls decls)
    sTree <- genTAC (NStatements stmnts)
    rAddr <- genTAC (NExpr ret)
    return $ Concat sTree (UInstr Return rAddr)

genTAC (NDecls (Decls' name typ)) = insert name typ
genTAC (NDecls (Decls decls name typ)) = do
  _ <- genTAC (NDecls decls)
  genTAC (NDecls (Decls' name typ))

genTAC (NExpr (Syntax.Var a)) = lookup a
genTAC (NExpr (Lit int)) = return $ IAddr (Val int)
genTAC (NExpr (Op a name expr)) = do
    lTree <- lookup name
    rTree <- genTAC (NExpr expr)
    (IAddr (Addr f)) <- fresh Int
    return $ BAssign f (BInstr a lTree rTree)

genTAC (NStatements (Statements' stmnt)) = genTAC (NStatement stmnt)
genTAC (NStatements (Statements stmnts stmnt)) = do
  stmnts' <- genTAC (NStatements stmnts)
  stmnt'  <- genTAC (NStatement stmnt)
  return $ Concat stmnts' stmnt'

genTAC (NStatement (SAssign name expr)) = do
    (IAddr (Addr a)) <- lookup name
    rTree <- genTAC (NExpr expr)
    return $ BAssign a rTree

genTAC (NStatement (SExpr expr)) = genTAC (NExpr expr)
genTAC (NStatement (SPrint expr)) = do
  eTree <- genTAC (NExpr expr)
  return $ UInstr Print eTree
