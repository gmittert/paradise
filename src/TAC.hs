module TAC
  (compile)
where

import Syntax
import Semantic
import Prelude hiding (lookup)
import qualified Data.Map.Strict as M
import Control.Monad

newtype CodeGen a = CodeGen { genCode :: CodegenState -> (a, CodegenState) }

instance Monad CodeGen where
  return a = CodeGen $ \s -> (a, s)
  m >>= k = CodeGen $ \s -> let (a, s') = genCode m s in
    genCode (k a) s'

instance Functor CodeGen where
  fmap = liftM

instance Applicative CodeGen where
  pure  = return
  (<*>) = ap

lookup :: Name -> CodeGen Addr
lookup name = CodeGen $ \state ->
  case M.lookup name (symtab state) of
    Just (Entry _ addr) -> (addr, state)
    Nothing -> error $ "Could not find " ++ toString name
      ++ " in table " ++ show state

insert :: Name -> Type -> CodeGen Addr
insert name typ = CodeGen $ \state ->
  (Addr (offset state), CodegenState { symtab = M.insert
                                       name
                                       (Entry typ (Addr (offset state)))
                                       (symtab state)
                       , nextTmp = nextTmp state
                       , offset = offset state + toSize typ
                       , instrs = instrs state})

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

fresh :: Type -> CodeGen Addr
fresh typ = CodeGen $ \state ->
  let freshName = (Name $ "$" ++ show (nextTmp state)) in
    (Addr (offset state), CodegenState {
        symtab = M.insert
                 freshName
                 (Entry typ (Addr (offset state)))
                 (symtab state)
        , nextTmp = nextTmp state + 1
        , offset = offset state + toSize typ
        , instrs = instrs state})

addConst :: Type -> Int -> CodeGen Addr
addConst typ val = CodeGen $ \state ->
  let freshName = (Name $ "$" ++ (show (nextTmp state))) in
    (Val val, CodegenState {
        symtab = M.insert
                 freshName
                 (Entry typ (Val val))
                 (symtab state)
        , nextTmp = nextTmp state + 1
        , offset = offset state + toSize typ
        , instrs = instrs state})

addInstrs :: [Instr] -> Addr -> CodeGen Addr
addInstrs nInstrs addr = CodeGen $ \state -> (addr, CodegenState {
      symtab = symtab state
    , nextTmp = nextTmp state
    , offset = offset state
    , instrs = instrs state ++ nInstrs
    })

compile :: Prog -> CodegenState
compile prog = let
  (_, state) = genCode (genTAC (NProg prog) (Addr 0))
    CodegenState {symtab = M.empty , nextTmp = 0 , offset  = 0 , instrs  = []}
  in state


genTAC :: SyntaxNode -> (Addr -> CodeGen Addr)
genTAC (NProg (Prog decls stmnts ret)) =
  \addr -> CodeGen $ \state -> let (rAddr, rState ) = genCode (genTAC (NDecls decls) addr >>= genTAC (NStatements stmnts) >>= genTAC (NExpr ret)) state in
    genCode (addInstrs [UInstr Return rAddr (Val (-1))] (Val (-1))) rState

genTAC (NDecls (Decls' name typ)) = \_ -> insert name typ

genTAC (NDecls (Decls decls name typ)) = genTAC (NDecls decls)
  >=> genTAC (NDecls (Decls' name typ))

genTAC (NExpr (Var a)) = \_ -> lookup a
genTAC (NExpr (Lit int)) = \_ -> CodeGen $ \state -> (Val int, state)
genTAC (NExpr (Op a name expr)) =
  \addr -> CodeGen $ \state -> let
    (nAddr, _) = genCode (lookup name) state
    (eAddr, _) = genCode (genTAC (NExpr expr) addr) state
    (fAddr, fState) = genCode (fresh Int) state in
      genCode (addInstrs [BInstr a nAddr eAddr fAddr] fAddr) fState

genTAC (NStatements (Statements' stmnt)) = genTAC (NStatement stmnt)
genTAC (NStatements (Statements stmnts stmnt)) =
  genTAC (NStatements stmnts) >=> genTAC (NStatement stmnt)

genTAC (NStatement (SAssign name expr)) =
  \addr -> CodeGen $ \state -> let
    (nAddr, _) = genCode (lookup name) state
    (eAddr, eState) = genCode (genTAC (NExpr expr) addr) state in
      genCode (addInstrs [BInstr Assign nAddr eAddr nAddr] nAddr) eState
genTAC (NStatement (SExpr expr)) = genTAC (NExpr expr)
genTAC (NStatement (SPrint expr)) = \addr -> CodeGen $ \state -> let
  (eAddr, eState ) = genCode (genTAC (NExpr expr) addr) state in
    genCode (addInstrs [UInstr Print eAddr (Val (-1))] (Val (-1))) eState
