module Canonicalizer where

import Lib.IR
import Control.Monad
import Control.Monad.Fix

{- The canonicalizer actually does several things
1. Rewrite the tree in to a list of canonical trees without Seq or Eseq
2. Move calls to a top level and assign the result to a temp
3. Linearize sequential statements
5. Order the blocks into traces so that every cjump is followed by its
   false label, allowing it to fall through
-}
canonicalize :: IRGen [Stm] -> Either String (IRGen [Stm])
canonicalize instrs = return $ instrs >>= elimSeq

-- | Return if a statement and an expression commute. That is, if stm does not
-- change the results of exp. We estimate this very conservatively
commute :: Stm -> Exp -> Bool
-- | Nops commute
commute (Sexp (Const _)) _ = True
-- | Constants commute
commute _ (Const _) = True
-- | Fixed Labels commute
commute _ (EName _) = True
-- | Nothing else commutes
commute _ _ = False

-- | We'd like to be able to reorder expressions as we please, but Stms and
-- therefore Eseqs can cause side effects which cannot be reordered. Therefore
-- we'd like to raise the Eseqs as high up in the tree as possible. Once we do
-- that, we return a tuple of ([Stm], [Exp]) where is the [Stm] contains all
-- the things that must be done before the [Exp] and the [Exp] which can then
-- be reordered as pleased.
elimSeq :: [Stm] -> IRGen [Stm]
elimSeq stmnts = forM stmnts (rewriteStmRec canonizeStm canonizeExp)

-- | Pull up an eseq in a stm
canonizeStm :: Stm -> IRGen Stm
-- | Pull eseqs out of jumps
canonizeStm (Jump (Computed (Eseq s e1)) labs) = return $ Seq s (Jump (Computed e1) labs)
canonizeStm (Cjump op (Eseq s e1) e2 l1 l2) = return $ Seq s (Cjump op e1 e2 l1 l2)
-- | If the statement is in the second expression, we have to store the first
-- expression before evaluating the statement
canonizeStm (Cjump op e1 (Eseq s e2) l1 l2) =
  let commutes = commute s e1 in
  -- If the statement commutes with the first expression, we are safe to move
  -- it in front of it
  if commutes
    then return $ Seq s (Cjump op e1 e2 l1 l2)
    else do
      t <- newTemp
      return $ Seq (Move t e1) (Seq s (Cjump op t e2 l1 l2))
canonizeStm (Seq (Sexp (Const _)) s1) = return s1
canonizeStm (Seq s1 (Sexp (Const _))) = return s1
canonizeStm (Seq (Seq a b) c) = return $ Seq a (Seq b c)
canonizeStm s = return s

-- | Pull up an eseq in an exp
canonizeExp :: Exp -> IRGen Exp
-- | Move nested ESeqs into one
canonizeExp (Eseq s1 (Eseq s2 e)) = return $ Eseq (Seq s1 s2) e
-- | Pull eseqs out of expressions
canonizeExp (Bop op (Eseq s e1) e2) = return $ Eseq s (Bop op e1 e2)
canonizeExp (Mem (Eseq s e1)) = return $ Eseq s (Mem e1)
-- | Move a statement out of a binary expression. But we have to make sure to
-- store the result of the first expression before we evaluate the statement
canonizeExp (Bop op e1 (Eseq s e2)) =
  let commutes = commute s e1 in
  -- If the statement commutes with the first expression, we are safe to move
  -- it in front of it
  if commutes
    then return $ Eseq s (Bop op e1 e2)
    else do
      t <- newTemp
      return $ Eseq (Move t e1) (Eseq s (Bop op t e2))
-- | Make sure to assign the result of a call to a variable. We replace the
-- Call node with an ACall node to indicate that it has been assigned.
canonizeExp (Call f args) = do
  t <- newTemp
  return $ Eseq (Move t (ACall f args)) t
canonizeExp e = return e
