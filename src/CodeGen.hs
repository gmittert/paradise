module CodeGen where

import Asm
import Optimize
import TAC
import Syntax
import Semantic
import Types
import Data.Char
import Data.Maybe
import qualified Data.Map.Strict as M

lookup' :: (Ord a, Show a) => a -> M.Map a b -> b
lookup' key m = fromMaybe
  (error $ show key ++ " was not in the map")
  (M.lookup key m)

codeGen :: (FlowGraph, CodegenState) -> [AInstr]
codeGen ((FlowGraph blk succ vars), state)
  = let varMap = getReg (FlowGraph blk succ vars) in
  [Globl "main", Label "main"
    , Comment "Allocate Frame"
    , Comment (show state)
    , Push Rax
    , Mov (SrcReg Rsp) (DestReg Rbp)
    , Sub (Asm.IInt (offset state)) (DestReg Rsp)
    , Comment "/Allocate Frame"
    ]
  ++ genTree blk varMap state
  ++ concatMap (\x -> codeGen (x, state)) succ

genTree :: TacTree -> M.Map Name (Maybe Reg) -> CodegenState -> [AInstr]
genTree (IStr name val) _ state = let (Entry typ (Addr addr)) = lookup' name ((vars.symTab) state ) in
  [Mov (Asm.IInt(length val)) (IDOffset (addr - 8))]
  ++ loadChar val (addr - 16)
  ++ [Mov (Asm.IInt (-1 * addr)) (DestReg Rax)]
  ++ [Add (SrcReg Rbp) (DestReg Rax)]
  ++ [Mov (SrcReg Rax) (IDOffset addr)]
  where
    loadChar [] _ = []
    loadChar (x:xs) addr = [Mov (Asm.IInt (ord x)) (IDOffset addr)] ++ loadChar xs (addr-1)
genTree (IName name ) _ state = let (Entry _ (Addr addr)) = lookup' name ((vars.symTab) state ) in
  [Mov (ISOffset addr) (DestReg Rax)]
genTree (IVal (Addr a)) _ _ = [Mov (ISOffset a) (DestReg Rax)]
genTree (IVal (Types.IInt a)) _ _ = [Mov (Asm.IInt a) (DestReg Rax)]
genTree (IVal (IChar a)) _ _ = [Mov (Asm.IInt (ord a)) (DestReg Rax)]
genTree (IVal (IBool True)) _ _ =
  [Mov (Asm.IInt 1) (DestReg Rax)]
genTree (IVal (IBool False)) _ _ =
  [Mov (Asm.IInt 0) (DestReg Rax)]
genTree (IVal (RelPtr a)) _ _ = [Mov (Asm.IInt a) (DestReg Rax)]
genTree (UInstr Neg tree) table state = genTree tree table state ++ [
  Mov (Asm.IInt 0) (DestReg Rbx)
  , Sub (SrcReg Rax) (DestReg Rbx)
  , Mov (SrcReg Rbx) (DestReg Rax)
  ]
genTree (UInstr Return tree) table state = genTree tree table state
  ++ [
  Comment "Returning"
  , Leave
  , Asm.Ret]
genTree (UInstr Print tree) table state =
  genTree tree table state
  -- We now have a pointer to the string in Rax
  -- The string itself looks like
  -- [8bytesAddr, 8bytessize, 1byte...]
  -- Move a pointer to the string to rsi
  ++ [Mov (SrcReg Rax) (DestReg Rsi)
     , Add  (Asm.IInt 16) (DestReg Rsi)
     -- 1 is to stdout
     , Mov (Asm.IInt 1) (DestReg Rdi)
     -- Move the length to Rdx
     , Mov (SrcRegPtr' Rax (-8)) (DestReg Rdx)
     -- 1 is sys_write
     , Mov (Asm.IInt 1) (DestReg Rax)
     , Syscall]
genTree (Concat l r) table state = genTree l table state ++ genTree r table state
genTree (BInstr op l r) table state = genTree r table state
  ++ [Comment "Binary op"]
  ++ genTree l table state
  ++ [Push Rax]
  ++ genTree r table state
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
genTree (BAssign name tree) table state =
  case lookup' name table of
    Just reg ->
      [ (Comment $ "assigning to " ++ show name)]
      ++ genTree tree table state
      ++ [Mov (SrcReg Rax) (DestReg reg)]
      ++ [ (Comment $ "/assigning to " ++ show name)]
    Nothing ->
      [ (Comment $ "assigning to " ++ show name)]
      ++ genTree tree table state
      ++ [Mov (SrcReg Rax) (IDOffset
                          (let (Entry _ (Addr int)) = (lookup' name ((vars.symTab) state)) in int))]
      ++ [ (Comment $ "/assigning to " ++ show name)]
-- For now, just spill all variables
-- whycantiholdallthesevariables.jpg
getReg :: FlowGraph -> M.Map Name (Maybe Reg)
getReg graph = foldr (\key map -> M.insert key Nothing map) M.empty (liveVars graph)
