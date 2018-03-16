module Lib.Blocks where
import qualified Ast.AddressedAst as AA

import Lib.IR
import Lib.Types

newtype Block = Block {stm :: [Stm]} deriving (Eq, Ord)
instance Show Block where
  show (Block stm) = "{\n"  ++ concatMap (\x -> show x ++ "\n") stm ++ "}\n"

-- All blocks must start with a label
getLabel :: Block -> Label
getLabel = firstLab . head . stm

firstLab :: Stm -> Label
firstLab (Lab l) = l
firstLab (Seq s1 _) = firstLab s1
firstLab (FPro (f@AA.Func{})) = (Label . show . AA.name) f
firstLab (FPro (f@AA.AsmFunc{})) = (Label . show . AA.name) f
firstLab a = error $ "Block did not start with a label " ++ show a

nextBlocks :: Block -> [Label]
nextBlocks = concatMap stmToJumps . stm

stmToJumps :: Stm -> [Label]
stmToJumps (Jump _ ls) = ls
stmToJumps (Cjump _ _ _ iftrue iffalse) = [iftrue, iffalse]
stmToJumps (Seq s1 s2) = stmToJumps s1 ++ stmToJumps s2
stmToJumps _  = []
