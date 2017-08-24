module Codegen where

import qualified Lib.IR as IR
import Asm
import Control.Monad
import Data.List
import Data.Maybe
import Data.Char
import Types
import qualified Data.Map as M

codegen :: [IR.Instr] -> Either String [AInstr]
codegen instrs = return $
  [ Globl "main"
  , Label "main"
  , Label "_start"
  , Push Rbp
  , Mov (SrcReg Rsp) (DestReg Rbp)] ++
  (join . map (ir2asm (allocate instrs)) $ instrs)

ir2asm :: M.Map IR.Var Location -> IR.Instr -> [AInstr]
ir2asm m (IR.FuncDef label) =
  [ Globl $ toString label
  , Label $ toString label]

ir2asm m (IR.Assign rval lval) =
  [Comment $ "Assigning " ++ show lval ++ " to " ++ show rval]
  ++ lval2asm m lval
  ++ (case rval of
     (IR.RVar v) -> let loc = fromJust $ M.lookup v m in
       [Mov (SrcReg Rax) (locToDest loc)]
     (IR.RAccess v1 v2) -> let
       loc1 = fromJust $M.lookup v1 m
       loc2 = fromJust $M.lookup v1 m in
       case loc1 of
         (Register r1) -> case loc2 of
           (Register r2) -> [Mov (SrcReg Rax) (DOffset 0 r1 r2 8)]
           (Memory i)    -> [ Mov (ISOffset i) (DestReg Rbx)
                            , Mov (SrcReg Rax) (DOffset 0 r1 Rbx 8)]
         (Memory i1) -> case loc2 of
           (Register r) -> [ Mov (SrcReg Rax) (DOffset (-8 * i1) Ebp r 8)]
           (Memory i2) -> [ Mov (ISOffset i2) (DestReg Rbx)
                          , Mov (SrcReg Rax) (DOffset 0 Ebp Rbx 8)])
ir2asm _ (IR.Goto l) = [Jmp (IR.label l)]
ir2asm m (IR.BrZero v l) = let loc = fromJust $ M.lookup v m in
  [Mov (locToSrc loc) (DestReg Rax)
  , Cmp  (IInt 0) (DestReg Rax)
  , Je (IR.label l)]
ir2asm _ (IR.Lab l) = [Asm.Label (IR.label l)]
ir2asm m (IR.Ret v) = let loc = fromJust $ M.lookup v m in [
  Mov (locToSrc loc) (DestReg Rax)
  , Pop Rbp
  , Ret
  ]

lval2asm :: M.Map IR.Var Location -> IR.LVal -> [Asm.AInstr]
lval2asm _ (IR.LInt i) = [Mov (IInt i) (DestReg Rax)]
lval2asm m (IR.IRVar v) = let loc = fromJust $ M.lookup v m in
  [Mov (locToSrc loc) (DestReg Rax)]
lval2asm m (IR.IRUOp Neg v) = let loc = fromJust $ M.lookup v m in
  [Mov (locToSrc loc) (DestReg Rbx)
  , Mov (IInt 0) (DestReg Rax)
  , Sub (SrcReg Rax) (DestReg Rbx)]
lval2asm m (IR.IRUOp Deref v) = let loc = fromJust $ M.lookup v m in
  [Mov (locToSrc loc) (DestReg Rbx)
  , Mov (SDeref (SrcReg Rax)) (DestReg Rbx)]
lval2asm m (IR.IRBOp Plus v1 v2) =
  let loc1 = fromJust $ M.lookup v1 m
      loc2 = fromJust $ M.lookup v2 m in
  [Mov (locToSrc loc1) (DestReg Rax)
  , Mov (locToSrc loc2) (DestReg Rbx)
  , Add (SrcReg Rbx) (DestReg Rax)]
lval2asm m (IR.IRBOp Minus v1 v2) =
  let loc1 = fromJust $ M.lookup v1 m
      loc2 = fromJust $ M.lookup v2 m in
  [Mov (locToSrc loc1) (DestReg Rax)
  , Mov (locToSrc loc2) (DestReg Rbx)
  , Sub (SrcReg Rbx) (DestReg Rax)]
lval2asm m (IR.IRBOp Times v1 v2) =
  let loc1 = fromJust $ M.lookup v1 m
      loc2 = fromJust $ M.lookup v2 m in
  [ Mov (locToSrc loc1) (DestReg Rax)
  , Mov (locToSrc loc2) (DestReg Rbx)
  , Imul (SrcReg Rbx)]
lval2asm m (IR.IRBOp Div v1 v2) =
  let loc1 = fromJust $ M.lookup v1 m
      loc2 = fromJust $ M.lookup v2 m in
  [ Mov (locToSrc loc1) (DestReg Rax)
  , CQO
  , Mov (locToSrc loc2) (DestReg Rbx)
  , Idiv (SrcReg Rbx)]
lval2asm m (IR.IRBOp Lt v1 v2) =
  let loc1 = fromJust $ M.lookup v1 m
      loc2 = fromJust $ M.lookup v2 m in
  [Mov (locToSrc loc1) (DestReg Rax)
  , Mov (locToSrc loc2) (DestReg Rbx)
  , Cmp (SrcReg Rbx) (DestReg Rax)
  , Setl (DestReg Al)
  , Movsx (DestReg Al) (DestReg Rax)
  ]
lval2asm m (IR.IRBOp Lte v1 v2) =
  let loc1 = fromJust $ M.lookup v1 m
      loc2 = fromJust $ M.lookup v2 m in
  [ Mov (locToSrc loc1) (DestReg Rax)
  , Mov (locToSrc loc2) (DestReg Rbx)
  , Cmp (SrcReg Rbx) (DestReg Rax)
  , Setle (DestReg Al)
  , Movsx (DestReg Al) (DestReg Rax)
  ]
lval2asm m (IR.IRBOp Access v1 v2) =
  let loc1 = fromJust $ M.lookup v1 m
      loc2 = fromJust $ M.lookup v2 m in
    [ Mov (locToSrc loc2) (DestReg Rbx)
    , Mov (IInt 8) (DestReg Rax)
    , Imul (SrcReg Rbx)
    , Mov (locToSrc loc1) (DestReg Rbx)
    , Add (SrcReg Rax) (DestReg Rbx)
    , Mov (SDeref (SrcReg Rax)) (DestReg Rax)]
lval2asm m (IR.IRArr i) = undefined

data Location
  = Register Asm.Reg
  | Memory Int

allocate :: [IR.Instr] -> M.Map IR.Var Location
allocate = let
  getVars = map IR.getRVal . filter IR.isAssign
  uniq = map head . group . sort
  allocateMem = flip zip (Memory <$> [8, 16 ..])
  in M.fromList . allocateMem . uniq . getVars

locToSrc :: Location -> Src
locToSrc loc = case loc of
      (Register reg) -> SrcReg reg
      (Memory i) -> ISOffset i

locToDest :: Location -> Dest
locToDest loc = case loc of
      (Register reg) -> DestReg reg
      (Memory i) -> IDOffset i
