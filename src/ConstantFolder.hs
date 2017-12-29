module ConstantFolder where

import Control.Monad

import Lib.IR
import Lib.Types

{- The constant folder calculates expressions at compile time and replaces them
with constants
-}
constFold :: IRGen [Stm] -> Either String (IRGen [Stm])
constFold instrs = return $ instrs >>= foldSeq

foldSeq :: [Stm] -> IRGen [Stm]
foldSeq stmnts = forM stmnts (rewriteStmRec foldStm foldExp)

-- | Reduces an expression to a constant
compute :: BinOp -> Int -> Int -> Exp
compute Plus l r = Const $ l + r
compute Minus l r = Const $ l - r
compute Div l r = Const $ quot l r
compute Times l r = Const $ l * r
compute Lt l r = Const $ if l < r then 1 else 0
compute Lte l r = Const $ if l <= r then 1 else 0
compute Gt l r = Const $ if l > r then 1 else 0
compute Gte l r = Const $ if l >= r then 1 else 0
compute Eq l r = Const $ if l == r then 1 else 0
compute Neq l r = Const $ if l /= r then 1 else 0
compute Access l r = Bop Access (Const l) (Const r)

foldStm :: Stm -> IRGen Stm
-- | Evaluate conditional jumps
foldStm (Cjump op (Const left) (Const right) true false)
  | compute op left right == Const 0 = return $ Jump (JLab false) [false]
  | otherwise = return $ Jump (JLab true) [true]
-- | Throw away Seqs with Expressions
foldStm (Seq s1 (Sexp _)) = return s1
foldStm (Sexp (Eseq s _)) = return s
foldStm a = return a

-- | Pull up an eseq in an exp
foldExp :: Exp -> IRGen Exp
foldExp (Bop b (Const l) (Const r)) = return $ compute b l r
foldExp (Bop Minus a (Const r)) = return $ Bop Plus a (Const ((-1) * r))
foldExp (Bop Plus (Bop Plus (Const l1) r1) (Const r2)) = return $ Bop Plus r1 (compute Plus l1 r2)
foldExp (Bop Plus (Bop Plus l1 (Const r1)) (Const r2)) = return $ Bop Plus l1 (compute Plus r1 r2)
foldExp (Bop Plus (Const l1) (Bop Plus (Const l2) r2)) = return $ Bop Plus r2 (compute Plus l1 l2)
foldExp (Bop Plus (Const l1) (Bop Plus l2 (Const r2))) = return $ Bop Plus l2 (compute Plus l1 r2)
foldExp (Bop Times (Bop Times (Const l1) r1) (Const r2)) = return $ Bop Times r1 (compute Times l1 r2)
foldExp (Bop Times (Bop Times l1 (Const r1)) (Const r2)) = return $ Bop Times l1 (compute Times r1 r2)
foldExp (Bop Times (Const l1) (Bop Times (Const l2) r2)) = return $ Bop Times r2 (compute Times l1 l2)
foldExp (Bop Times (Const l1) (Bop Times l2 (Const r2))) = return $ Bop Times l2 (compute Times l1 r2)
foldExp a = return a
