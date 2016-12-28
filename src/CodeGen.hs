module CodeGen where

import Asm
import Optimize
import TAC
import Syntax
import Semantic
import Types
import qualified Data.Map.Strict as M

codeGen :: (FlowGraph, CodegenState) -> [AInstr]
codeGen ((FlowGraph blk succ vars), state)
  = let varMap = getReg (FlowGraph blk succ vars) in
  [Globl "main", Label "main"
    , Comment "Allocate Frame"
    , Push Rax
    , Mov (SrcReg Rsp) (DestReg Rbp)
    , Sub (IInt (offset state)) (DestReg Rsp)
    , Comment "/Allocate Frame"
    ]
  ++ genTree blk varMap
  ++ concatMap (\x -> codeGen (x, state)) succ

genTree :: TacTree -> M.Map Name (Maybe Reg) -> [AInstr]
genTree (IAddr (Addr a)) map = [Mov (ISOffset a) (DestReg Rax)]
genTree (IAddr (Val a)) map = [Mov (IInt a) (DestReg Rax)]
genTree (UInstr Neg tree) map = genTree tree map ++ [
  Mov (IInt 0) (DestReg Rbx)
  , Sub (SrcReg Rax) (DestReg Rbx)
  , Mov (SrcReg Rbx) (DestReg Rax)
  ]
genTree (UInstr Return tree) map = genTree tree map
  ++ [
  Comment "Returning"
  , Leave
  , Ret]
genTree (UInstr Print tree) map = undefined
genTree (Concat l r) map = genTree l map ++ genTree r map
genTree (BInstr op l r) map = genTree r map
  ++ [Comment "Binary op"]
  ++ genTree l map
  ++ [Push Rax]
  ++ genTree r map
  ++ [Mov (SrcReg Rax) (DestReg Rbx)]
  ++ [Pop Rax]
  ++ bop
  ++ [Comment "/Binary op"]
  where bop = case op of
          Plus -> [Add (SrcReg Rbx) (DestReg Rax)]
          Minus -> [Sub (SrcReg Rbx) (DestReg Rax)]
          Times -> undefined
          Div -> undefined
          Assign -> undefined
genTree (BAssign addr tree) map =
  [ (Comment $ "assigning to " ++ show addr)]
  ++ genTree tree map
  ++ [Mov (SrcReg Rax) (IDOffset addr)]
  ++ [ (Comment $ "/assigning to " ++ show addr)]
-- For now, just spill all variables
-- whycantiholdallthesevariables.jpg
getReg :: FlowGraph -> M.Map Name (Maybe Reg)
getReg graph = foldr (\key map -> M.insert key Nothing map) M.empty (liveVars graph)
