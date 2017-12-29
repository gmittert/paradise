module Codegen where

import qualified Lib.IR as IR
import qualified Ast.AddressedAst as AA
import Control.Monad.State.Lazy
import Lib.Asm
import Data.Maybe
import Lib.Types
import qualified Data.Map as M
import BasicBlocks

codegen :: IR.IRGen [Block] -> Either String (IR.IRGen [AInstr])
codegen funcs = return $ funcs >>= genBlocks

genBlocks :: [Block] -> IR.IRGen [AInstr]
genBlocks funcs = join <$> forM funcs genBlock

genBlock :: Block -> IR.IRGen [AInstr]
genBlock (Block stm) = join <$> forM stm stm2asm

getVarSrc :: Name -> IR.IRGen Src
getVarSrc v = do
  locals <- IR.currLocals <$> get
  let errMsg = error ("Internal Compiler error: Failed to find '" ++ show v ++ "'  in " ++ show (M.toList locals))
  return $ addrToSrc $ fromMaybe errMsg (M.lookup v locals)

getVarDest :: Name -> IR.IRGen Dest
getVarDest v = do
  locals <- IR.currLocals <$> get
  let errMsg = error ("Internal Compiler error: Failed to find '" ++ show v ++ "'  in " ++ show (M.toList locals))
  return $ addrToDest $ fromMaybe errMsg (M.lookup v locals)

exp2asm :: IR.Exp -> IR.IRGen [AInstr]
exp2asm (IR.Const i) = return [Mov (IInt i) (DestReg Rax)]
exp2asm (IR.EName (Lib.Types.Label l)) = return [Mov (SLabel l) (DestReg Rax)]
exp2asm t@(IR.Temp _) = do
  src <- getVarSrc (IR.tempToName t)
  return [Mov src (DestReg Rax)]
exp2asm (IR.Arg i) = return [Mov (addrToSrc (Lib.Types.Arg i)) (DestReg Rax)]
exp2asm IR.FP = return [Mov (SrcReg Rbp) (DestReg Rax)]
exp2asm (IR.Bop op exp1 exp2) = do
  e2 <- (++ [Push Rax]) <$> exp2asm exp2
  e1 <- exp2asm exp1
  let code = e2 ++ e1 ++ [Pop Rbx]
  return $ code ++ (case op of
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
exp2asm (IR.Mem (IR.Bop Plus IR.FP (IR.Const c))) = return [Mov (ISOffset c) (DestReg Rax)]
exp2asm (IR.Mem exp) = do
  e <- exp2asm exp
  return $ e ++ [Mov (SDeref (SrcReg Rax)) (DestReg Rax)]
exp2asm (IR.Call (IR.EName (Lib.Types.Label l)) args) = (++ [Lib.Asm.Call l]) <$>  pushReg args
exp2asm (IR.ACall (IR.EName (Lib.Types.Label l)) args) = (++ [Lib.Asm.Call l]) <$>  pushReg args
exp2asm (IR.Eseq s e) = do
  stm <- stm2asm s
  exp <- exp2asm e
  return $ stm ++ exp

movReg :: [AInstr]
movReg = Pop <$> [Rdi, Rsi, Rdx, Rcx, R8, R9]

-- | Generates the instructions to prepare to call a function
pushReg :: [IR.Exp] -> IR.IRGen [AInstr]
pushReg addrs = do
  computeArgs <- join <$> forM (reverse addrs) (fmap (++ [Push Rax]) . exp2asm)
  return $ computeArgs ++ take (length addrs) movReg

stm2asm :: IR.Stm -> IR.IRGen [AInstr]
stm2asm (IR.Move (IR.Bop Plus IR.FP (IR.Const c1)) (IR.Const c2)) = return [Mov (IInt c2) (IDOffset c1)]
stm2asm (IR.Move (IR.Bop Plus IR.FP (IR.Const c)) e2) = do
  exp2 <- exp2asm e2
  return $ exp2 ++ [Mov (SrcReg Rax) (IDOffset c)]
stm2asm (IR.Move t@(IR.Temp _) e2) = do
  e2' <- exp2asm e2
  dest <- getVarDest (IR.tempToName t)
  return $ e2' ++ [Mov (SrcReg Rax) dest]
stm2asm (IR.Move e1 e2) = do
  exp1 <- exp2asm e1
  exp2 <- exp2asm e2
  return $ exp1 ++ [Push Rax] ++ exp2 ++ [Pop Rbx] ++ [Mov (SrcReg Rax) (DDeref (DestReg Rbx))]
stm2asm (IR.Sexp e) = exp2asm e
stm2asm (IR.Ret e) = exp2asm e
stm2asm (IR.Jump e _) = case e of
  IR.JLab (Lib.Types.Label l) -> return [Jmp (SLabel l)]
  IR.Computed e -> (++ [Jmp (SrcReg Rax)]) <$>  exp2asm e
stm2asm (IR.Cjump op left right (Lib.Types.Label iftrue) (Lib.Types.Label iffalse)) = do
  right <- (++ [Push Rax]) <$> exp2asm right
  left <- (++ [Pop Rbx]) <$> exp2asm left
  return $ right ++ left ++ [Cmp (SrcReg Rax)(SrcReg Rbx), (case op of
        Lt -> Jl
        Lte -> Jle
        Gt -> Jg
        Gte -> Jge
        Eq -> Je
        Neq -> Jne
        _ -> error "Not a comp operator") (SLabel iftrue), Jmp (SLabel iffalse)]
stm2asm (IR.Seq s1 s2) = do
  s1 <- stm2asm s1
  s2 <- stm2asm s2
  return $ s1 ++ s2
stm2asm (IR.Lab (Lib.Types.Label l)) = return [Lib.Asm.Label l]
stm2asm (IR.FPro (AA.Func _ (Name name) _ _ offset _)) =
  return [ Lib.Asm.Label name
         , Push Rbp
         , Mov (SrcReg Rsp) (DestReg Rbp)
         , Sub (IInt (-1 * offset)) (DestReg Rsp)
         ]
stm2asm (IR.FEpi (AA.Func _ (Name name) _ _ offset _)) =
  return $ [ Add (IInt (-1 * offset)) (DestReg Rsp)
         , Pop Rbp ] ++
    if name == "main"
    then [
      Mov (SrcReg Rax) (DestReg Rdi)
    , Mov (IInt 60) (DestReg Rax)
    , Syscall
    ] else [Ret]

delim :: String -> [AInstr] -> [AInstr]
delim blk instrs = Comment blk : instrs ++ [Comment $ "/" ++ blk]
