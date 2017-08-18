module BasicBlocks where

import Lib.IR

{- For now, we just put everything into the same block -}
assignBlocks :: [IRInstr] -> Either String [(IRInstr, Int)]
assignBlocks instrs = return $ map (\x -> (x, 0)) instrs
