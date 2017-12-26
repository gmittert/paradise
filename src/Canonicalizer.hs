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
elimSeq stmnts = forM stmnts rewriteStmRec

-- Apply the rewrite rules over and over recursively until we get no change
rewriteStmRec :: Stm -> IRGen Stm
rewriteStmRec = fix rewriteStm

rewriteStm :: (Stm -> IRGen Stm) -> (Stm -> IRGen Stm)
rewriteStm r stm@(Move e1 e2) = do
  e1 <- (rewriteExpNode <=< rewriteExpRec) e1
  e2 <- (rewriteExpNode <=< rewriteExpRec) e2
  stm2 <- rewriteStmNode (Move e1 e2)
  if stm == stm2 then return stm else r stm2
rewriteStm r stm@(Sexp e1) = do
  e1 <- (rewriteExpNode <=< rewriteExpRec) e1
  stm2 <- rewriteStmNode (Sexp e1)
  if stm == stm2 then return stm else r stm2
rewriteStm r stm@(Jump e1 lbl) = case e1 of
  Computed e -> do
    e1 <- (rewriteExpNode <=< rewriteExpRec) e
    stm2 <- rewriteStmNode (Jump (Computed e1) lbl)
    if stm == stm2 then return stm else r stm2
  JLab _ -> return stm

rewriteStm r stm@(Cjump rop e1 e2 t f) = do
  e1 <- (rewriteExpNode <=< rewriteExpRec) e1
  e2 <- (rewriteExpNode <=< rewriteExpRec) e2
  stm2 <- rewriteStmNode (Cjump rop e1 e2 t f)
  if stm == stm2 then return stm else r stm2
--rewriteStm _ (Seq (Sexp (Const _)) s1) = (rewriteStmNode <=< rewriteStmRec) s1
--rewriteStm _ (Seq s1 (Sexp (Const _))) = (rewriteStmNode <=< rewriteStmRec) s1
rewriteStm _ (Seq (Seq a b) c) = return $ Seq a (Seq b c)
rewriteStm r stm@(Seq s1 s2) = do
  s1 <- (rewriteStmNode <=< rewriteStmRec) s1
  s2 <- (rewriteStmNode <=< rewriteStmRec) s2
  stm2 <- rewriteStmNode (Seq s1 s2)
  if stm == stm2 then return stm else r stm2
rewriteStm _ a = return a

-- | Pull up an eseq in a stm
rewriteStmNode :: Stm -> IRGen Stm
-- | Pull eseqs out of jumps
rewriteStmNode (Jump (Computed (Eseq s e1)) labs) = return $ Seq s (Jump (Computed e1) labs)
rewriteStmNode (Cjump op (Eseq s e1) e2 l1 l2) = return $ Seq s (Cjump op e1 e2 l1 l2)
-- | If the statement is in the second expression, we have to store the first
-- expression before evaluating the statement
rewriteStmNode (Cjump op e1 (Eseq s e2) l1 l2) =
  let commutes = commute s e1 in
  -- If the statement commutes with the first expression, we are safe to move
  -- it in front of it
  if commutes
    then return $ Seq s (Cjump op e1 e2 l1 l2)
    else do
      t <- newTemp
      return $ Seq (Move t e1) (Seq s (Cjump op t e2 l1 l2))
rewriteStmNode s = return s

-- Apply the rewrite rules over and over recursively until we get no change
rewriteExpRec :: Exp -> IRGen Exp
rewriteExpRec = fix rewriteExp

rewriteExp :: (Exp -> IRGen Exp) -> (Exp -> IRGen Exp)
rewriteExp r exp@(Bop op e1 e2) = do
  e1 <- (rewriteExpNode <=< rewriteExpRec) e1
  e2 <- (rewriteExpNode <=< rewriteExpRec) e2
  exp2 <- rewriteExpNode (Bop op e1 e2)
  if exp2 == exp then return exp else r exp2
rewriteExp r exp@(Mem e) = do
  e <- (rewriteExpNode <=< rewriteExpRec) e
  exp2 <- rewriteExpNode (Mem e)
  if exp2 == exp then return exp else r exp2
rewriteExp r exp@(Call f args) = do
  f <- rewriteExpNode f
  args <- forM args rewriteExpNode
  exp2 <- rewriteExpNode (Call f args)
  if exp2 == exp then return exp else r exp2
rewriteExp r exp@(Eseq s e) = do
  s <- rewriteStmNode s
  e <- rewriteExpNode e
  exp2 <- rewriteExpNode (Eseq s e)
  if exp2 == exp then return exp else r exp2
rewriteExp _ a = return a

-- | Pull up an eseq in an exp
rewriteExpNode :: Exp -> IRGen Exp
-- | Move nested ESeqs into one
rewriteExpNode (Eseq s1 (Eseq s2 e)) = return $ Eseq (Seq s1 s2) e
-- | Pull eseqs out of expressions
rewriteExpNode (Bop op (Eseq s e1) e2) = return $ Eseq s (Bop op e1 e2)
rewriteExpNode (Mem (Eseq s e1)) = return $ Eseq s (Mem e1)
-- | Move a statement out of a binary expression. But we have to make sure to
-- store the result of the first expression before we evaluate the statement
rewriteExpNode (Bop op e1 (Eseq s e2)) =
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
rewriteExpNode (Call f args) = do
  t <- newTemp
  return $ Eseq (Move t (ACall f args)) t
rewriteExpNode e = return e
