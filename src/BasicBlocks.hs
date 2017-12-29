module BasicBlocks where

import Lib.IR
import Control.Monad

newtype Block = Block {stm :: [Stm]} deriving (Eq, Ord)
instance Show Block where
  show (Block stm) = "{\n"  ++ concatMap (\x -> show x ++ "\n") stm ++ "}\n"

-- | Group in to a set of basic blocks which have no jumps or labels
basicBlocks :: IRGen [Stm] -> Either String (IRGen [Block])
basicBlocks stms = return $ stms >>= doBlocks

doBlocks :: [Stm] -> IRGen [Block]
doBlocks stms = return $ concat $ stmToBlock <$> stms

stmToBlock :: Stm -> [Block]
stmToBlock stm = case makeBlock stm of
  (a, Sexp (Const 0)) -> [a]
  (a, b) -> a : stmToBlock b

-- | Returns the first basic block in a statement and the remaining unblocked
-- part of the statement
makeBlock :: Stm -> (Block, Stm)
-- | We end the block when we see a jump
makeBlock (Seq l@Jump{} r) = (Block [l], r)
makeBlock (Seq l@Cjump{} r) = (Block [l], r)
-- | If we see a label, we end the current block, but make sure we add a jump to
-- the next block
makeBlock (Seq l r@(Seq (Lab lab) _)) = (Block [l, Jump (JLab lab) [lab]], r)
-- | Non control flow means we continue building the block
makeBlock (Seq l r) = let (a, b) = makeBlock r in (Block (l : stm a), b)
-- | When we get to the end of the sequence, return a nop as a the left over
makeBlock a = (Block [a], Sexp (Const 0))
