{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TAC
  (compile)
where

import Syntax
import Semantic
import Prelude hiding (lookup)
import qualified Data.Map.Strict as M
import Control.Monad.State.Lazy

newtype CodeGen a = CodeGen { genCode :: State CodegenState a }
  deriving (Functor, Applicative, Monad, MonadState CodegenState)


data UniOp = Neg | Print | Return
  deriving (Eq, Ord, Show)

data Instr
  = BInstr BinOp Addr Addr Addr
  | UInstr UniOp Addr Addr
  deriving (Eq, Ord, Show)

data CodegenState
  = CodegenState {
      symtab :: SymbolTable
    , nextTmp :: Int
    , offset  :: Int
    , instrs   :: [Instr]
    }
  deriving (Eq, Ord, Show)

lookup :: Name -> CodeGen Addr
lookup name = CodeGen . state $ \s ->
  case M.lookup name (symtab s) of
    Just (Entry _ addr) -> (addr, s)
    Nothing -> error $ "Could not find " ++ toString name
      ++ " in table " ++ show s

insert :: Name -> Type -> CodeGen Addr
insert name typ = CodeGen $ state $ \s ->
  (Addr (offset s), CodegenState
                    (M.insert name (Entry typ (Addr (offset s))) (symtab s))
                    (nextTmp s)
                    (offset s + toSize typ)
                    (instrs s))
fresh :: Type -> CodeGen Addr
fresh typ = CodeGen $ state $ \s ->
  (Addr (offset s), CodegenState
        (M.insert
          (Name $ "$" ++ show (nextTmp s))
          (Entry typ (Addr (offset s)))
          (symtab s))
        (nextTmp s + 1)
        (offset s + toSize typ)
        (instrs s))

addInstrs :: [Instr] -> CodeGen ()
addInstrs nInstrs =
  modify $ \s -> CodegenState {
      symtab = symtab s
    , nextTmp = nextTmp s
    , offset = offset s
    , instrs = instrs s ++ nInstrs
    }

compile :: Prog -> CodegenState
compile prog = execState (genCode $ genTAC (NProg prog))
    CodegenState {symtab = M.empty , nextTmp = 0 , offset  = 0 , instrs  = []}


genTAC :: SyntaxNode -> CodeGen Addr
genTAC (NProg (Prog decls stmnts ret)) = do
    _ <- genTAC (NDecls decls)
    _ <- genTAC (NStatements stmnts)
    rAddr <- genTAC (NExpr ret)
    addInstrs [UInstr Return rAddr (Val (-1))]
    return (error "Attempted to evaluate the result of a program")

genTAC (NDecls (Decls' name typ)) = insert name typ
genTAC (NDecls (Decls decls name typ)) = do
  _ <- genTAC (NDecls decls)
  genTAC (NDecls (Decls' name typ))

genTAC (NExpr (Var a)) = lookup a
genTAC (NExpr (Lit int)) = return (Val int)
genTAC (NExpr (Op a name expr)) = do
    nAddr <- lookup name
    eAddr <- genTAC (NExpr expr)
    fAddr <- fresh Int
    addInstrs [BInstr a nAddr eAddr fAddr]
    return fAddr

genTAC (NStatements (Statements' stmnt)) = genTAC (NStatement stmnt)
genTAC (NStatements (Statements stmnts stmnt)) = do
  _ <- genTAC (NStatements stmnts)
  genTAC (NStatement stmnt)

genTAC (NStatement (SAssign name expr)) = do
    nAddr <-lookup name
    eAddr <- genTAC (NExpr expr)
    addInstrs [BInstr Assign nAddr eAddr nAddr]
    return nAddr

genTAC (NStatement (SExpr expr)) = genTAC (NExpr expr)
genTAC (NStatement (SPrint expr)) = do
  eAddr <- genTAC (NExpr expr)
  addInstrs [UInstr Print eAddr (Val (-1))]
  return (error "Attempted to evaluate the result of a print statement")
