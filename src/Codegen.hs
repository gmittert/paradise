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
getVar v m = fromMaybe (error ("Internal Compiler error: Failed to find '" ++ show v ++ "'  in " ++ show (M.toList m))) (M.lookup v m)

ir2asm :: M.Map IR.Var Location -> IR.Instr -> [AInstr]

{-
  If we do this right, our stack should look like
  | fparam n   |  Higher memory
  | fparam n-1 |
  |     ...    |
  | fparam 2   |
  | fparam 1   |
  | old rip    |
  | old rbp    | <- rbp
  | localvar 1 |
  | localvar 2 |
  | localvar 3 | <- rsp
                 Lower memory
-}
ir2asm m (IR.Func label args) = let
  space = spaceUsed m in
  [ Globl $ toString label
  , Label $ toString label]
  ++ (if toString label == "func__main" then
  [ Globl "main"
   , Label "main"
   --, Globl "_start"
   , Label "_start"] else []) ++
  -- Save the callers base pointer
  [ Push Rbp
  -- Move the base pointer to the top of the stack
  , Mov (SrcReg Rsp) (DestReg Rbp)
  -- Allocate space for locals
  , Add (IInt (-1 * (space + 8))) (DestReg Rsp)
  ]

ir2asm m (IR.Assign lval rval) =
  [Comment $ "Assigning " ++ show lval ++ " to " ++ show rval]
  ++ rval2asm m rval
  ++ (case lval of
     (IR.LVar v) -> let loc = getVar v m in
        [Mov (SrcReg Rax) (locToDest loc)]
     (IR.LAccess v1 v2) -> let
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

rval2asm :: M.Map IR.Var Location -> IR.RVal -> [Asm.AInstr]
rval2asm _ (IR.RInt i) = [Mov (IInt i) (DestReg Rax)]
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
rval2asm m (IR.IRArr v _) = let loc = getVar v m in
  case loc of
    Memory i -> [ Mov (SrcReg Rbp) (DestReg Rax)
                , Add (IInt ((-1* i) + 8)) (DestReg Rax)]
rval2asm m (IR.IRVar v) = let loc = getVar v m in
  [Mov (locToSrc loc) (DestReg Rax)]
rval2asm m (IR.IRUOp Types.Neg v) = let loc = getVar v m in
  [Mov (locToSrc loc) (DestReg Rax)
  , Asm.Neg (DestReg Rax)]
rval2asm m (IR.IRUOp Deref v) = let loc = getVar v m in
  [Mov (locToSrc loc) (DestReg Rbx)
  , Mov (SDeref (SrcReg Rax)) (DestReg Rbx)]
rval2asm m (IR.IRBOp Plus v1 v2) =
  let loc1 = getVar v1 m
      loc2 = getVar v2 m in
  [Mov (locToSrc loc1) (DestReg Rax)
  , Mov (locToSrc loc2) (DestReg Rbx)
  , Add (SrcReg Rbx) (DestReg Rax)]
rval2asm m (IR.IRBOp Minus v1 v2) =
  let loc1 = getVar v1 m
      loc2 = getVar v2 m in
  [Mov (locToSrc loc1) (DestReg Rax)
  , Mov (locToSrc loc2) (DestReg Rbx)
  , Sub (SrcReg Rbx) (DestReg Rax)]
rval2asm m (IR.IRBOp Times v1 v2) =
  let loc1 = getVar v1 m
      loc2 = getVar v2 m in
  [ Mov (locToSrc loc1) (DestReg Rax)
  , Mov (locToSrc loc2) (DestReg Rbx)
  , Imul (SrcReg Rbx)]
rval2asm m (IR.IRBOp Div v1 v2) =
  let loc1 = getVar v1 m
      loc2 = getVar v2 m in
  [ Mov (locToSrc loc1) (DestReg Rax)
  , CQO
  , Mov (locToSrc loc2) (DestReg Rbx)
  , Idiv (SrcReg Rbx)]
rval2asm m (IR.IRBOp Lt v1 v2) =
  let loc1 = getVar v1 m
      loc2 = getVar v2 m in
  [Mov (locToSrc loc1) (DestReg Rax)
  , Mov (locToSrc loc2) (DestReg Rbx)
  , Cmp (SrcReg Rbx) (DestReg Rax)
  , Setl (DestReg Al)
  , Movsx (DestReg Al) (DestReg Rax)
  ]
rval2asm m (IR.IRBOp Lte v1 v2) =
  let loc1 = getVar v1 m
      loc2 = getVar v2 m in
  [ Mov (locToSrc loc1) (DestReg Rax)
  , Mov (locToSrc loc2) (DestReg Rbx)
  , Cmp (SrcReg Rbx) (DestReg Rax)
  , Setle (DestReg Al)
  , Movsx (DestReg Al) (DestReg Rax)
  ]
rval2asm m (IR.IRBOp Access arr idx) =
  let arrloc = getVar arr m
      idxloc = getVar idx m in
    [ Mov (locToSrc arrloc) (DestReg Rax)
    , Mov (locToSrc idxloc) (DestReg Rbx)
    , Mov (SOffset 0 Rax Rbx 8) (DestReg Rax)]
rval2asm m (IR.Call name vars) =
  -- First push parameters onto the stack in reverse order
  join (map (\x -> let loc = getVar x m in
          [Mov (locToSrc loc) (DestReg Rax)
          , Push Rax]) (reverse vars))
  ++
  -- Call the function
  [ Call $ IR.label name
  , Sub (IInt (8 * length vars)) (DestReg Rsp)
  ]

data Location
  = Register Asm.Reg
  | Memory Int
  deriving (Eq, Show)

spaceUsed :: M.Map IR.Var Location -> Int
spaceUsed = foldr (\x y -> case snd x of
                             Register {} -> y
                             Memory i -> max i y) 0 . M.toList

allocate :: [IR.Instr] -> M.Map IR.Var Location
allocate instrs = let
  -- The first instruction should be the function definition
  args = case head instrs of
    (IR.Func _ a) -> a
    _ -> error "First instr wasn't the func def"
  getVars = map IR.getLVal . filter IR.isAssign
  uniq = map head . group . sort
  allocateArgs = foldr
    (\def m -> case fst def of
      -- If it's array, we make room for both the pointer and the data
      Arr {} -> (fst m - 8, M.insert (IR.Var (toString (snd def)) (fst def)) (Memory (fst m - 8)) (snd m))
      _ -> (fst m - toSize (fst def), M.insert (IR.Var (toString (snd def)) (fst def)) (Memory (fst m - toSize (fst def))) (snd m))
    ) (-8, M.empty)
  allocateMem = foldr
    (\def m ->
        case def of
          IR.LVar (IR.Var name tpe) -> case tpe of
            -- If it's array, we make room for both the pointer and the data
            Arr {} -> (fst m + toSize tpe + 8, M.insert (IR.Var name tpe) (Memory (fst m)) (snd m))
            _ -> (fst m + toSize tpe, M.insert (IR.Var name tpe) (Memory (fst m)) (snd m))
          IR.LAccess {} -> m
            ) (8, M.empty)
  in (snd . allocateMem . uniq . getVars) instrs `M.union` (snd . allocateArgs ) args

locToSrc :: Location -> Src
locToSrc loc = case loc of
      (Register reg) -> SrcReg reg
      (Memory i) -> ISOffset i

locToDest :: Location -> Dest
locToDest loc = case loc of
      (Register reg) -> DestReg reg
      (Memory i) -> IDOffset i
