{- |
Module      : Reachbility
Description : Trace through the blocks and remove any that cannot be reached
Copyright   : (c) Jason Mittertreiner, 2018
-}
module Reachability where

import qualified Data.Map as M
import Control.Monad.State.Lazy
import Data.Maybe

import Lib.IR
import Lib.Blocks
import Lib.Types

-- |Trace through the blocks and remove any that cannot be reached
--
-- We'll do this in two parts. First we scan through the blocks and for each
-- labeled jump we see, we add the corresponding block to the list.
reachability :: IRGen [Block] -> Either String (IRGen [Block])
reachability blks = return $ blks >>= doReaching

doReaching :: [Block] -> IRGen [Block]
doReaching [] = return []
doReaching bs = do
  (Just mainName) <- mainFunc <$> get
  return $ snd $ trace (blocksToMap bs) (findMain (Label (show mainName)) bs)

findMain :: Label -> [Block] -> Block
findMain l bs = case filter ((== l) . getLabel) bs of
  [] -> error $ "Could not find main with label: " ++ show l
  a:_ -> a


-- | Create a mapping from labels to blocks
blocksToMap :: [Block] -> M.Map Label Block
blocksToMap = foldr (\x y -> M.insert (getLabel x) x y) M.empty

trace :: M.Map Label Block -> Block -> (M.Map Label Block, [Block])
trace m b = let jmps = nextBlocks b ++ concatMap scalls (stm b)
                blocks = catMaybes $ flip M.lookup m <$> jmps
                m' = foldr M.delete m jmps
                next = foldr (\b acc -> (snd acc ++) <$> trace (fst acc) b) (m', []) blocks
  in (b :) <$> next

-- | We need to know all the places that a stm/exp can call
ecalls :: Exp -> [Label]
ecalls (EName l _) = [l]
ecalls (Bop _ e1 e2 _) = ecalls e1 ++ ecalls e2
ecalls (Uop _ e _) = ecalls e
ecalls (Mem e _) = ecalls e
ecalls (Call f args _ _) = ecalls f ++ concatMap ecalls args
ecalls (ACall f args _ _) = ecalls f ++ concatMap ecalls args
ecalls (Eseq s e _) = scalls s ++ ecalls e
ecalls _  = []

scalls :: Stm -> [Label]
scalls (Move e1 e2) = ecalls e1 ++ ecalls e2
scalls (Sexp e) = ecalls e
scalls (Ret e) = ecalls e
scalls (Jump (Computed e) _) = ecalls e
scalls (Cjump _ left right _ _) = ecalls left ++ ecalls right
scalls (Seq s1 s2) = scalls s1 ++ scalls s2
scalls _ = []
