module Codegen where

import Lib.IR
import Asm
import Control.Monad
import Data.List
import Data.Maybe
import Data.Char
import Types
import qualified Data.Map as M

codegen :: [IRInstr] -> Either String [AInstr]
codegen instrs = return $ [Globl "main"
                 , Asm.Label "main"
                 , Push Rbp
                 , Mov (SrcReg Rsp) (DestReg Rbp)] ++
                  (join . map (ir2asm (allocate instrs)) $ instrs)
                  ++ [Pop Rbp, Ret]



ir2asm :: M.Map Var Location -> IRInstr -> [AInstr]
ir2asm m (IRAssign v lval) = let loc = fromJust $ M.lookup v m in
  lval2asm m lval ++ [Mov (SrcReg Rax) (locToDest loc)]
ir2asm _ (IRGoto l) = undefined
ir2asm _ (IRBrZero v l) = undefined
ir2asm _ (IRLabel l) = [Asm.Label (label l)]

lval2asm :: M.Map Var Location -> IRLVal -> [Asm.AInstr]
lval2asm _ (IRChar c) = [Mov (IInt (ord c)) (DestReg Rax)]
lval2asm _ (IRInt i) = [Mov (IInt i) (DestReg Rax)]
lval2asm m (IRVar v) = let loc = fromJust $ M.lookup v m in
  [Mov (locToSrc loc) (DestReg Rax)]
lval2asm m (IRUOp Neg v) = let loc = fromJust $ M.lookup v m in
  [Mov (locToSrc loc) (DestReg Rbx)
  , Mov (IInt 0) (DestReg Rax)
  , Sub (SrcReg Rax) (DestReg Rbx)]
lval2asm m (IRUOp Deref v) = let loc = fromJust $ M.lookup v m in
  [Mov (locToSrc loc) (DestReg Rbx)
  , Mov (SDeref (SrcReg Rax)) (DestReg Rbx)]
lval2asm m (IRBOp Plus v1 v2) =
  let loc1 = fromJust $ M.lookup v1 m
      loc2 = fromJust $ M.lookup v2 m in
  [Mov (locToSrc loc1) (DestReg Rax)
  , Mov (locToSrc loc2) (DestReg Rbx)
  , Add (SrcReg Rax) (DestReg Rbx)]
lval2asm m (IRBOp Minus v1 v2) =
  let loc1 = fromJust $ M.lookup v1 m
      loc2 = fromJust $ M.lookup v2 m in
  [Mov (locToSrc loc1) (DestReg Rax)
  , Mov (locToSrc loc2) (DestReg Rbx)
  , Sub (SrcReg Rbx) (DestReg Rax)]
lval2asm m (IRBOp Times v1 v2) =
  let loc1 = fromJust $ M.lookup v1 m
      loc2 = fromJust $ M.lookup v2 m in
  [Mov (locToSrc loc1) (DestReg Rax)
  , Mov (locToSrc loc2) (DestReg Rbx)
  , Mul (SrcReg Rax) (DestReg Rbx)]
lval2asm m (IRBOp Types.Div v1 v2) =
  let loc1 = fromJust $ M.lookup v1 m
      loc2 = fromJust $ M.lookup v2 m in
  [Mov (locToSrc loc1) (DestReg Rax)
  , Mov (locToSrc loc2) (DestReg Rbx)
  , Asm.Div (SrcReg Rax) (DestReg Rbx)]
lval2asm m (IRBOp Lt v1 v2) = undefined
lval2asm m (IRBOp Lte v1 v2) = undefined
lval2asm m (IRBOp Access v1 v2) = undefined

data Location
  = Register Asm.Reg
  | Memory Int

allocate :: [IRInstr] -> M.Map Var Location
allocate = let
  getVars = map getRVal . filter isAssign
  uniq = map head . group . sort
  allocateMem = flip zip (Memory <$> [8, 16 ..])
  in M.fromList . allocateMem . uniq . getVars

locToSrc :: Location -> Src
locToSrc loc = case loc of
      (Register reg) -> (SrcReg reg)
      (Memory i) -> (ISOffset i)

locToDest :: Location -> Dest
locToDest loc = case loc of
      (Register reg) -> (DestReg reg)
      (Memory i) -> (IDOffset i)
