{- |
Module      : IR
Description : An intermediate representation based off of the Tree IR in Appel's
              Modern Compiler Implementation
Copyright   : (c) Jason Mittertreiner, 2017
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Lib.IR where
import Lib.Types
import Control.Monad.State.Lazy
import qualified Data.Map as M
import qualified Ast.AddressedAst as AA

data GenIRState = GenIRState {
  -- | The name of the function we're generating
  currFunc :: QualifiedName
  -- | The next label name we'll make
  , nextLabel :: Int
  -- | The next temp name we'll make
  , nextTemp :: Int
  -- | The memory location where we'll put the next tmp
  , nextOffset :: Int
  -- | Dictionary of the locals we have
  , currLocals :: M.Map Name Address
}

-- Given a starting memory offset, create an empty state
emptyState :: GenIRState
emptyState = GenIRState (mkQName (ModulePath []) (Name "")) 0 0 0 M.empty

newtype IRGen a = IRGen { irgen :: State GenIRState a}
  deriving (Functor, Applicative, Monad, MonadState GenIRState)

newLabel :: IRGen Label
newLabel = do
  modify $ \st -> st{nextLabel = nextLabel st + 1}
  st <- get
  return $ Label $ "L" ++ show (nextLabel st)

-- Generate a name from a temp
tempToName :: Exp -> Name
tempToName t@(Temp _) = Name $ show t
tempToName _ = error "This isn't a Temp"

-- Generate a new temporary
newTemp :: IRGen Exp
newTemp = do
  currtmp <- nextTemp <$> get
  currOffset <- nextOffset <$> get
  modify $ \st -> st{ nextTemp = currtmp + 1
                    , nextOffset = currOffset - 8
                    , currLocals = M.insert (Name ("$t" ++ show currtmp)) (Offset currOffset) (currLocals st)}
  return $ Temp currtmp

-- Mark a function as the currently focused function
setFunc :: AA.Function -> IRGen ()
setFunc f = modify $ \st -> st{ currFunc = AA.name f
                              , currLocals = AA.locals f
                              , nextOffset = AA.nextOffset f
                              }

-- | Compute a value (with side effects)
data Exp
  -- | An integer constant
  = Const Int
  -- | A symbolic constant/label
  | EName Label
  -- | A temporary var
  | Temp Int
  -- | The ith argument
  | Arg Int
  -- | A special variable: the frame pointer
  | FP
  -- | Binary operation (note that we don't have unary operations)
  | Bop BinOp Exp Exp
  -- | Get the memory contents of exp
  | Mem Exp
  -- | Function call on the result of func
  | Call {func :: Exp, args:: [Exp]}
  -- | Function call on the result of func that is assigned to a var
  | ACall {func :: Exp, args:: [Exp]}
  -- | Evaluate Stm then Exp
  | Eseq Stm Exp
  deriving (Eq, Ord)
instance Show Exp where
  show (Const i) = "$" ++ show i
  show (EName l) = show l
  show (Temp i) = "$t" ++ show i
  show (Lib.IR.Arg i) = "arg" ++ show i
  show FP = "FP"
  show (Bop op e1 e2) = show e1 ++ " " ++ show op ++ " " ++ show e2
  show (Mem e) = "Mem(" ++ show e ++ ")"
  show (Call f args) = show f ++ " " ++ show args
  show (ACall f args) = show f ++ " " ++ show args
  show (Eseq s e) = "Eseq " ++ show s ++ ", " ++ show e

-- | Operations that do side effects and control flow
data Stm
  -- | Evaluate e1 then e2. Move e2 into e1
  = Move {e1 :: Exp, e2 :: Exp}
  -- | Eval Exp and discard
  | Sexp Exp
  -- | Eval Exp and return
  | Ret Exp
  -- | Jump the the address at Exp, can be a literal label or computed address
  -- [Label] lists all possible locations Exp can evaluate to
  | Jump JumpTarget [Label]
  -- | Evaluate left, then right, the compare with relop and take the appropriate jump
  | Cjump {relop :: BinOp, left :: Exp, right :: Exp, iftrue:: Label, iffalse :: Label}
  -- | Eval s1 then s2
  | Seq {s1 :: Stm, s2 :: Stm}
  -- | Label definition/target of jumps
  | Lab Label
  -- | Designates the prologue of a function
  | FPro AA.Function
  -- | Designates the epilogue of a function
  | FEpi AA.Function
  deriving (Eq, Ord)
instance Show Stm where
  show (Move e1 e2) = show e1 ++ " <- " ++ show e2
  show (Sexp e) = show e
  show (Ret e) = "return " ++ show e
  show (Jump e _) = "Jump " ++ show e
  show (Cjump o l r t f) = "CJump " ++ show l ++ " " ++ show o ++ " " ++ show r ++ " " ++ show t ++ " " ++ show f
  show (Seq l r) = "Seq " ++ show l ++ "\n" ++ show r
  show (Lab l) = show l ++ ":"
  show (FPro (AA.Func _ n _ _ _ _ )) = "begin " ++ show n
  show (FEpi (AA.Func _ n _ _ _ _ )) = "end " ++ show n

data JumpTarget
  = Computed Exp
  | JLab Lib.Types.Label
  deriving (Eq, Ord)

instance Show JumpTarget where
  show (Computed e) = show e
  show (JLab l) = show l

-- Apply the rewrite rules over and over recursively until we get no change
rewriteStmRec :: (Stm -> IRGen Stm)
  -> (Exp -> IRGen Exp)
  -> (Stm -> IRGen Stm)
rewriteStmRec stmf expf = fix (\x -> rewriteStm x stmf expf)

rewriteStm :: (Stm -> IRGen Stm)
  -> (Stm -> IRGen Stm)
  -> (Exp -> IRGen Exp)
  -> (Stm -> IRGen Stm)
rewriteStm r stmf expf stm@(Move e1 e2) = do
  e1 <- (expf <=< rewriteExpRec stmf expf) e1
  e2 <- (expf <=< rewriteExpRec stmf expf) e2
  stm2 <- stmf (Move e1 e2)
  if stm == stm2 then return stm else r stm2
rewriteStm r stmf expf stm@(Sexp e1) = do
  e1 <- (expf <=< rewriteExpRec stmf expf) e1
  stm2 <- stmf (Sexp e1)
  if stm == stm2 then return stm else r stm2
rewriteStm r stmf expf stm@(Jump e1 lbl) = case e1 of
  Computed e -> do
    e1 <- (expf <=< rewriteExpRec stmf expf) e
    stm2 <- stmf (Jump (Computed e1) lbl)
    if stm == stm2 then return stm else r stm2
  JLab _ -> return stm
rewriteStm r stmf expf stm@(Cjump rop e1 e2 t f) = do
  e1 <- (expf <=< rewriteExpRec stmf expf) e1
  e2 <- (expf <=< rewriteExpRec stmf expf) e2
  stm2 <- stmf (Cjump rop e1 e2 t f)
  if stm == stm2 then return stm else r stm2
rewriteStm r stmf expf stm@(Seq s1 s2) = do
  s1 <- (stmf <=< rewriteStmRec stmf expf) s1
  s2 <- (stmf <=< rewriteStmRec stmf expf) s2
  stm2 <- stmf (Seq s1 s2)
  if stm == stm2 then return stm else r stm2
rewriteStm _ _ _ a = return a

-- Apply the rewrite rules over and over recursively until we get no change
rewriteExpRec :: (Stm -> IRGen Stm)
  -> (Exp -> IRGen Exp)
  -> (Exp -> IRGen Exp)
rewriteExpRec stmf expf = fix (\x -> rewriteExp x stmf expf)

rewriteExp :: (Exp -> IRGen Exp)
  -> (Stm -> IRGen Stm)
  -> (Exp -> IRGen Exp)
  -> (Exp -> IRGen Exp)
rewriteExp r stmf expf exp@(Bop op e1 e2) = do
  e1 <- (expf <=< rewriteExpRec stmf expf) e1
  e2 <- (expf <=< rewriteExpRec stmf expf) e2
  exp2 <- expf (Bop op e1 e2)
  if exp2 == exp then return exp else r exp2
rewriteExp r stmf expf exp@(Mem e) = do
  e <- (expf <=< rewriteExpRec stmf expf) e
  exp2 <- expf (Mem e)
  if exp2 == exp then return exp else r exp2
rewriteExp r _ expf exp@(Call f args) = do
  f <- expf f
  args <- forM args expf
  exp2 <- expf (Call f args)
  if exp2 == exp then return exp else r exp2
rewriteExp r stmf expf exp@(Eseq s e) = do
  s <- stmf s
  e <- expf e
  exp2 <- expf (Eseq s e)
  if exp2 == exp then return exp else r exp2
rewriteExp _ _ _ a = return a

