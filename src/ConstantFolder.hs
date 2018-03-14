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
compute :: BinOp -> Size -> Int -> Int -> Exp
compute Plus s l r = Const (l + r) s
compute Minus s l r = Const (l - r) s
compute Div s l r = Const (quot l r) s
compute Times s l r = Const (l * r) s
compute Lt s l r = Const (if l < r then 1 else 0) s
compute Lte s l r = Const (if l <= r then 1 else 0) s
compute Gt s l r = Const (if l > r then 1 else 0) s
compute Gte s l r = Const (if l >= r then 1 else 0) s
compute Eq s l r = Const (if l == r then 1 else 0) s
compute Neq s l r = Const (if l /= r then 1 else 0) s
compute Access s l r = Bop Access (Const l 8) (Const r 8) s

foldStm :: Stm -> IRGen Stm
-- | Evaluate conditional jumps
foldStm (Cjump op (Const left sz1) (Const right sz2) true false)
  | compute op sz1 left right == Const 0 8 = return $ Jump (JLab false) [false]
  | otherwise = return $ Jump (JLab true) [true]
-- | Throw away Seqs with Expressions
foldStm (Seq s1 (Sexp _)) = return s1
foldStm (Sexp (Eseq s _ _)) = return s
foldStm a = return a

-- | Pull up an eseq in an exp
foldExp :: Exp -> IRGen Exp
foldExp (Bop b (Const l sz1) (Const r sz2) sz3) = return $ compute b sz3 l r
foldExp (Bop Minus a (Const r sz1) sz2) = return $ Bop Plus a (Const ((-1) * r) sz1) sz2
foldExp (Bop Plus (Bop Plus (Const l1 sz1) r1 sz2) (Const r2 sz3) sz4) = return $ Bop Plus r1 (compute Plus sz2 l1 r2) sz4
foldExp (Bop Plus (Bop Plus l1 (Const r1 sz1) sz2) (Const r2 sz3) sz4) = return $ Bop Plus l1 (compute Plus sz3 r1 r2) sz4
foldExp (Bop Plus (Const l1 sz1) (Bop Plus (Const l2 sz2) r2 sz3) sz4) = return $ Bop Plus r2 (compute Plus sz2 l1 l2) sz4
foldExp (Bop Plus (Const l1 sz1) (Bop Plus l2 (Const r2 sz2) sz3) sz4) = return $ Bop Plus l2 (compute Plus sz3 l1 r2) sz4
foldExp (Bop Plus a (Const 0 _) sz) =  return a
foldExp (Bop Plus (Const 0 _) a sz) =  return a

foldExp (Bop Times (Bop Times (Const l1 sz1) r1 sz2) (Const r2 sz3) sz4) = return $ Bop Times r1 (compute Times sz2 l1 r2) sz4
foldExp (Bop Times (Bop Times l1 (Const r1 sz1) sz2) (Const r2 sz3) sz4) = return $ Bop Times l1 (compute Times sz2 r1 r2) sz4
foldExp (Bop Times (Const l1 sz1) (Bop Times (Const l2 sz2) r2 sz3) sz4) = return $ Bop Times r2 (compute Times sz1 l1 l2) sz1
foldExp (Bop Times (Const l1 sz1) (Bop Times l2 (Const r2 sz2) sz3) sz4) = return $ Bop Times l2 (compute Times sz2 l1 r2) sz4
foldExp (Bop Times a (Const 1 _) _) =  return a
foldExp (Bop Times _ (Const 0 _) _) = return (Const 0 8)
foldExp (Bop Times (Const 1 _) a _) = return a
foldExp (Bop Div a (Const 1 _) _) =  return a
foldExp (Bop Div _ (Const 0 _) _) =  error "detected div by zero error"
foldExp a = return a
