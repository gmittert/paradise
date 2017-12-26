module Codegen where

import qualified Lib.IR as IR
import Lib.Asm
import Control.Monad
import Data.Maybe
import Lib.Types
import qualified Data.Map as M
import BasicBlocks

codegen :: IR.IRGen [Block] -> Either String (IR.IRGen [AInstr])
codegen funcs = return $ funcs >>= genBlocks

genBlocks :: [Block] -> IR.IRGen [AInstr]
genBlocks funcs = join <$> forM funcs genBlock

genBlock :: Block -> IR.IRGen [AInstr]
genBlock (Block stm) = return $ join (stm2asm <$> stm)

getVar :: Int -> M.Map Int Src -> Src
getVar v m = fromMaybe (error ("Internal Compiler error: Failed to find '" ++ show v ++ "'  in " ++ show (M.toList m))) (M.lookup v m)

getArg :: Int -> Src
getArg _ = undefined

exp2asm :: IR.Exp -> [AInstr]
exp2asm (IR.Const i) = [Mov (IInt i) (DestReg Rax)]
exp2asm (IR.EName (Lib.Types.Label l)) = [Mov (SLabel l) (DestReg Rax)]
exp2asm (IR.Temp i) = [Mov (getVar i undefined) (DestReg Rax)]
exp2asm (IR.Arg i) = [Mov (getArg i) (DestReg Rax)]
exp2asm IR.FP = [Mov (SrcReg Rbp) (DestReg Rax)]
exp2asm (IR.Bop op exp1 exp2) = exp2asm exp2 ++ [Push Rax] ++ exp2asm exp1 ++ [Pop Rbx] ++
  (case op of
    Plus -> [Add (SrcReg Rbx) (DestReg Rax)]
    Minus -> [Sub (SrcReg Rbx) (DestReg Rax)]
    Times -> [Imul (SrcReg Rbx)]
    Div -> [ CQO
           , Idiv (SrcReg Rbx)]
    Lt -> [ Cmp (SrcReg Rbx) (SrcReg Rax)
          , Setl (DestReg Al)
          , Movsx (DestReg Al) (DestReg Rax)]
    Lte -> [ Cmp (SrcReg Rbx) (SrcReg Rax)
           , Setle (DestReg Al)
           , Movsx (DestReg Al) (DestReg Rax)]
    Gt -> [ Cmp (SrcReg Rbx) (SrcReg Rax)
          , Setg (DestReg Al)
          , Movsx (DestReg Al) (DestReg Rax)]
    Gte -> [ Cmp (SrcReg Rbx) (SrcReg Rax)
           , Setge (DestReg Al)
           , Movsx (DestReg Al) (DestReg Rax)]
    Access -> [Mov (SOffset 0 Rax Rbx 8) (DestReg Rax)]
    Eq -> undefined
    Neq -> undefined)
exp2asm (IR.Mem exp) = exp2asm exp ++ [Mov (SDeref (SrcReg Rax)) (DestReg Rax)]
exp2asm (IR.Call (IR.EName (Lib.Types.Label l)) args) = pushReg args ++ [Lib.Asm.Call l]
exp2asm (IR.ACall (IR.EName (Lib.Types.Label l)) args) = pushReg args ++ [Lib.Asm.Call l]
exp2asm (IR.Eseq s e) = stm2asm s ++ exp2asm e

movReg :: [AInstr]
movReg = Pop <$> [Rdi, Rsi, Rdx, Rcx, R8, R9]

-- | Generates the instructions to prepare to call a function
pushReg :: [IR.Exp] -> [AInstr]
pushReg addrs = join (map (\x -> exp2asm x ++ [Push Rax]) (reverse addrs))
  ++ take (length addrs) movReg

stm2asm :: IR.Stm -> [AInstr]
stm2asm (IR.Move e1 e2) = (case e1 of
  -- When we're assigning, we want an L value, so ignore the mem
  IR.Mem a -> exp2asm a
  _ -> undefined) ++ [Push Rax] ++ exp2asm e2 ++ [Pop Rbx] ++ [Mov (SrcReg Rax) (DDeref (DestReg Rbx))]
stm2asm (IR.Sexp e) = exp2asm e
stm2asm (IR.Jump e _) = case e of
  IR.JLab (Lib.Types.Label l) -> [Jmp (SLabel l)]
  IR.Computed e -> exp2asm e ++ [Jmp (SrcReg Rax)]
stm2asm (IR.Cjump op left right (Lib.Types.Label iftrue) (Lib.Types.Label iffalse)) = exp2asm right ++ [Push Rax] ++ exp2asm left ++ [Pop Rbx] ++ [Cmp (SrcReg Rax)(SrcReg Rbx), (case op of
        Lt -> Jl
        Lte -> Jle
        Gt -> Jg
        Gte -> Jge
        Eq -> Je
        Neq -> Jne) (SLabel iftrue), Jmp (SLabel iffalse)]
stm2asm (IR.Seq s1 s2) = stm2asm s1 ++ stm2asm s2
stm2asm (IR.Lab (Lib.Types.Label l)) = [Lib.Asm.Label l]
stm2asm (IR.FPro (Name name) _ locals) =
  let localSize = M.foldr (\x y -> toSize x + y) 0 locals in
  [ Lib.Asm.Label name
  , Push Rbp
  , Mov (SrcReg Rsp) (DestReg Rbp)
  , Sub (IInt localSize) (DestReg Rsp)
  ]
stm2asm (IR.FEpi (Name name) _ locals) =
  let localSize = M.foldr (\x y -> toSize x + y) 0 locals in
  [ Add (IInt localSize) (DestReg Rsp)
  , Pop Rbp ] ++
  if name == "main"
  then [
    Mov (SrcReg Rax) (DestReg Rdi)
    , Mov (IInt 60) (DestReg Rax)
    , Syscall
    ] else [Ret]
