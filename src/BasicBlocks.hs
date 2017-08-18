module BasicBlocks where

import qualified Lib.IR as IR

{- For now, we just put everything into the same block -}
assignBlocks :: [IR.Instr] -> Either String [(IR.Instr, Int)]
assignBlocks instrs = return $ map (\x -> (x, 0)) instrs
