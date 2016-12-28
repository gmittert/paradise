module CodeGen where

import Asm
import Optimize
import TAC
import Syntax
import Semantic
import Types
import qualified Data.Map.Strict as M

codeGen :: FlowGraph -> [AInstr]
codeGen (FlowGraph blk succ vars) = genTree blk (getReg (FlowGraph blk succ vars)) ++ concatMap codeGen succ

genTree :: TacTree -> M.Map Name (Maybe Reg) -> [AInstr]
genTree (IAddr (Addr a)) map = [Movl (ISOffset a) (DestReg Eax)]
genTree (IAddr (Val a)) map = [Movl (IInt a) (DestReg Eax)]
genTree (UInstr Neg tree) map = genTree tree map ++ [Subl (IInt 0) (DestReg Eax)]
genTree (UInstr Return tree) map = genTree tree map ++ [Ret]
genTree (UInstr Print tree) map = undefined
genTree (Concat l r) map = genTree l map ++ genTree r map
genTree (BInstr op l r) map = genTree r map ++ [Movl (SrcReg Eax) (DestReg Ebx)] ++ genTree l map ++ bop
  where bop = case op of
          Plus -> [Addl (SrcReg Eax) (DestReg Ebx)]
          Minus -> [Subl (SrcReg Eax) (DestReg Ebx)]
          Times -> undefined
          Div -> undefined
genTree (BAssign addr tree) map = genTree tree map ++ [Movl (SrcReg Eax) (IDOffset addr)]
-- For now, just spill all variables
-- whycantiholdallthesevariables.jpg
getReg :: FlowGraph -> M.Map Name (Maybe Reg)
getReg graph = foldr (\key map -> M.insert key Nothing map) M.empty (liveVars graph)
