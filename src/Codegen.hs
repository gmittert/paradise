module Codegen where

import qualified Lib.IR as IR
import Asm
import Control.Monad
import Data.List
import Data.Maybe
import Types
import qualified Data.Map as M

codegen :: [[IR.Instr]] -> Either String [AInstr]
codegen funcs = return $ join (join (zipWith (\allocs func -> map (ir2asm allocs) func) (map allocate funcs) funcs))

getVar :: IR.Var -> M.Map IR.Var Location -> Location
getVar v m = fromMaybe (error ("Internal Compiler error: Failed to find " ++ show v ++ " in " ++ show (M.toList m))) (M.lookup v m)

ir2asm :: M.Map IR.Var Location -> IR.Instr -> [AInstr]
ir2asm m (IR.Func label) = let
  space = spaceUsed m in
  [ Globl $ toString label
   , Label $ toString label]
  ++ (if toString label == "func__main" then
  [ Globl "main"
   , Label "main"
   --, Globl "_start"
   , Label "_start"] else []) ++
  [ Push Rbp
  , Mov (SrcReg Rsp) (DestReg Rbp)
  , Add (IInt (-1 * (space + 8))) (DestReg Rsp)
  ]

ir2asm m (IR.Assign rval lval) =
  [Comment $ "Assigning " ++ show lval ++ " to " ++ show rval]
  ++ lval2asm m lval
  ++ (case rval of
     (IR.RVar v) -> let loc = getVar v m in
        [Mov (SrcReg Rax) (locToDest loc)]
     (IR.RAccess v1 v2) -> let
       arrVar = getVar v1 m
       idxVar = getVar v2 m in
       case arrVar of
         (Register r1) -> case idxVar of
           (Register r2) -> [Mov (SrcReg Rax) (DOffset 0 r1 r2 8)]
           (Memory i)    -> [ Mov (ISOffset i) (DestReg Rbx)
                            , Mov (SrcReg Rax) (DOffset i r1 Rbx 8)]
         (Memory i1) -> case idxVar of
           (Register r) -> [ Mov (SrcReg Rax) (DOffset (-1 * i1) Rbp r 8)]
           (Memory i2) -> [ Mov (ISOffset i1) (DestReg Rbx)
                          , Mov (ISOffset i2) (DestReg Rcx)
                          , Mov (SrcReg Rax) (DOffset 0 Rbx Rcx 8)])
ir2asm _ (IR.Goto l) = [Jmp (IR.label l)]
ir2asm m (IR.BrZero v l) = let loc = getVar v m in
  [Mov (locToSrc loc) (DestReg Rax)
  , Cmp  (IInt 0) (DestReg Rax)
  , Je (IR.label l)]
ir2asm _ (IR.Lab l) = [Asm.Label (IR.label l)]
ir2asm m (IR.Ret v) = let
  loc = getVar v m
  space = spaceUsed m in [
  Mov (locToSrc loc) (DestReg Rax)
  , Mov (SrcReg Rbp) (DestReg Rsp)
  , Pop Rbp
  , Ret
  ]

lval2asm :: M.Map IR.Var Location -> IR.LVal -> [Asm.AInstr]
lval2asm _ (IR.LInt i) = [Mov (IInt i) (DestReg Rax)]
{-
When we assign an array literal to a variable, we allocate space for both the
pointer and the data on the stack. The pointer then points to the first item of
the array. Note that it's stored "upside down" so that incrementing pointers
works out properly.

0xfff8  | 4       |
0xfff0  | 3       |
0xffe8  | 2       |
0xffe0  | 1       |
0xffd8  | 0       |<----\
0xffd0  | 0xffd8  |-----/

Then when we assign the array to other variables, we assign the pointer
-}
lval2asm m (IR.IRArr v _) = let loc = getVar v m in
  case loc of
    Memory i -> [ Mov (SrcReg Rbp) (DestReg Rax)
                , Add (IInt ((-1* i) + 8)) (DestReg Rax)]
lval2asm m (IR.IRVar v) = let loc = getVar v m in
  [Mov (locToSrc loc) (DestReg Rax)]
lval2asm m (IR.IRUOp Types.Neg v) = let loc = getVar v m in
  [Mov (locToSrc loc) (DestReg Rax)
  , Asm.Neg (DestReg Rax)]
lval2asm m (IR.IRUOp Deref v) = let loc = getVar v m in
  [Mov (locToSrc loc) (DestReg Rbx)
  , Mov (SDeref (SrcReg Rax)) (DestReg Rbx)]
lval2asm m (IR.IRBOp Plus v1 v2) =
  let loc1 = getVar v1 m
      loc2 = getVar v2 m in
  [Mov (locToSrc loc1) (DestReg Rax)
  , Mov (locToSrc loc2) (DestReg Rbx)
  , Add (SrcReg Rbx) (DestReg Rax)]
lval2asm m (IR.IRBOp Minus v1 v2) =
  let loc1 = getVar v1 m
      loc2 = getVar v2 m in
  [Mov (locToSrc loc1) (DestReg Rax)
  , Mov (locToSrc loc2) (DestReg Rbx)
  , Sub (SrcReg Rbx) (DestReg Rax)]
lval2asm m (IR.IRBOp Times v1 v2) =
  let loc1 = getVar v1 m
      loc2 = getVar v2 m in
  [ Mov (locToSrc loc1) (DestReg Rax)
  , Mov (locToSrc loc2) (DestReg Rbx)
  , Imul (SrcReg Rbx)]
lval2asm m (IR.IRBOp Div v1 v2) =
  let loc1 = getVar v1 m
      loc2 = getVar v2 m in
  [ Mov (locToSrc loc1) (DestReg Rax)
  , CQO
  , Mov (locToSrc loc2) (DestReg Rbx)
  , Idiv (SrcReg Rbx)]
lval2asm m (IR.IRBOp Lt v1 v2) =
  let loc1 = getVar v1 m
      loc2 = getVar v2 m in
  [Mov (locToSrc loc1) (DestReg Rax)
  , Mov (locToSrc loc2) (DestReg Rbx)
  , Cmp (SrcReg Rbx) (DestReg Rax)
  , Setl (DestReg Al)
  , Movsx (DestReg Al) (DestReg Rax)
  ]
lval2asm m (IR.IRBOp Lte v1 v2) =
  let loc1 = getVar v1 m
      loc2 = getVar v2 m in
  [ Mov (locToSrc loc1) (DestReg Rax)
  , Mov (locToSrc loc2) (DestReg Rbx)
  , Cmp (SrcReg Rbx) (DestReg Rax)
  , Setle (DestReg Al)
  , Movsx (DestReg Al) (DestReg Rax)
  ]
lval2asm m (IR.IRBOp Access arr idx) =
  let arrloc = getVar arr m
      idxloc = getVar idx m in
    [ Mov (locToSrc arrloc) (DestReg Rax)
    , Mov (locToSrc idxloc) (DestReg Rbx)
    , Mov (SOffset 0 Rax Rbx 8) (DestReg Rax)]

data Location
  = Register Asm.Reg
  | Memory Int
  deriving (Eq, Show)

spaceUsed :: M.Map IR.Var Location -> Int
spaceUsed = foldr (\x y -> case snd x of
                             Register {} -> y
                             Memory i -> max i y) 0 . M.toList

allocate :: [IR.Instr] -> M.Map IR.Var Location
allocate = let
  getVars = map IR.getRVal . filter IR.isAssign
  uniq = map head . group . sort
  allocateMem = foldr
    (\def m ->
        case def of
          IR.RVar (IR.Var name tpe) -> case tpe of
            -- If it's array, we make room for both the pointer and the data
            Arr {} -> (fst m + toSize tpe + 8, M.insert (IR.Var name tpe) (Memory (fst m)) (snd m))
            _ -> (fst m + toSize tpe, M.insert (IR.Var name tpe) (Memory (fst m)) (snd m))
          IR.RAccess {} -> m
            ) (8, M.empty)
  in snd . allocateMem . uniq . getVars

locToSrc :: Location -> Src
locToSrc loc = case loc of
      (Register reg) -> SrcReg reg
      (Memory i) -> ISOffset i

locToDest :: Location -> Dest
locToDest loc = case loc of
      (Register reg) -> DestReg reg
      (Memory i) -> IDOffset i
