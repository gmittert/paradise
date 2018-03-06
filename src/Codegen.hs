module Codegen where

import Control.Monad.State.Lazy
import Data.Maybe
import Control.Applicative

import qualified Lib.IR as IR
import qualified Ast.AddressedAst as AA
import Lib.Asm
import Lib.Types
import qualified Data.Map as M
import BasicBlocks

codegen :: IR.IRGen [Block] -> Either String (IR.IRGen [AInstr])
codegen funcs = return $ funcs >>= genBlocks >>= genExtern

genExtern :: [AInstr] -> IR.IRGen [AInstr]
genExtern i = return $ Extern "alloc":i

genBlocks :: [Block] -> IR.IRGen [AInstr]
genBlocks funcs = join <$> forM funcs genBlock

genBlock :: Block -> IR.IRGen [AInstr]
genBlock (Block stm) = join <$> forM stm stm2asm

getVar :: (Address -> a) -> Name -> IR.IRGen a
getVar convert v = do
  locals <- IR.currLocals <$> get
  temps <- IR.temps <$> get
  let errMsg = error ("Internal Compiler error: Failed to find '" ++ show v ++ "'  in " ++ show (M.toList locals))
  return $ convert $ fromMaybe errMsg (M.lookup v locals <|> M.lookup v temps)

getVarSrc :: Name -> IR.IRGen Src
getVarSrc = getVar addrToSrc

getVarDest :: Name -> IR.IRGen Dest
getVarDest = getVar addrToDest

exp2asm :: IR.Exp -> IR.IRGen [AInstr]
exp2asm (IR.Const i) = return [Mov (IInt i) (DestReg Rax)]
exp2asm (IR.EName (Lib.Types.Label l)) = return [Mov (SLabel l) (DestReg Rax)]
exp2asm t@(IR.Temp _) = do
  src <- getVarSrc (IR.tempToName t)
  return [Mov src (DestReg Rax)]
exp2asm (IR.RegArg i s) = return [Mov (addrToSrc (Lib.Types.RegArg i s)) (DestReg Rax)]
exp2asm (IR.StackArg i s) = return [Mov (addrToSrc (Lib.Types.StackArg i s)) (DestReg Rax)]
exp2asm IR.FP = return [Mov (SrcReg Rbp) (DestReg Rax)]
exp2asm (IR.Uop Lib.Types.Neg exp1) = do
  e1 <- exp2asm exp1
  return $ e1 ++ [Lib.Asm.Neg (SrcReg Rax)]
exp2asm (IR.Uop Lib.Types.Not exp1) = do
  e1 <- exp2asm exp1
  return $ e1 ++ [Xor (IInt 1) (DestReg Rax)]
exp2asm (IR.Uop Alloc exp1) = do
  e1 <- exp2asm exp1
  return $ e1 ++ [Call "__alloc"]
exp2asm (IR.Uop Len exp1) = do
  e1 <- exp2asm exp1
  return $ e1 ++ [
     Mov (RegOffset 16 Rax) (DestReg Rax),
     Mov (SDeref (SrcReg Rax)) (DestReg Rax)
     ]

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
    Eq -> [ Cmp (SrcReg Rbx) (SrcReg Rax)
           , Sete (DestReg Al)
           , Movsx (DestReg Al) (DestReg Rax)]
    Neq -> [ Cmp (SrcReg Rbx) (SrcReg Rax)
           , Setne (DestReg Al)
           , Movsx (DestReg Al) (DestReg Rax)])
exp2asm (IR.Mem (IR.Bop Plus IR.FP (IR.Const c))) = return [Mov (ISOffset c) (DestReg Rax)]
exp2asm (IR.Mem exp) = do
  e <- exp2asm exp
  return $ e ++ [Mov (SDeref (SrcReg Rax)) (DestReg Rax)]
exp2asm (IR.Call (IR.EName (Lib.Types.Label l)) args addrs) = do
  genArgs <- pushReg args addrs
  return $ saveReg ++ genArgs ++ [Lib.Asm.Call l] ++ restoreReg

exp2asm (IR.ACall (IR.EName (Lib.Types.Label l)) args addrs) = do
  genArgs <- pushReg args addrs
  return $ saveReg ++ genArgs ++ [Lib.Asm.Call l] ++ restoreReg
exp2asm (IR.Eseq s e) = do
  stm <- stm2asm s
  exp <- exp2asm e
  return $ stm ++ exp

-- | Arguments are passed in this order to these registers in the SystemV AMD64
--   ABI. Additional args are passed on the stack
argReg = [Rdi, Rsi, Rdx, Rcx, R8, R9]

-- | We don't know which registers we need to save before a function call,
-- so we save and restore all of them
saveReg :: [AInstr]
saveReg = Push <$> argReg

restoreReg :: [AInstr]
restoreReg = Pop <$> reverse argReg

-- | Generates the instructions to prepare to call a function
pushReg :: [IR.Exp] -> [Address] -> IR.IRGen [AInstr]
pushReg exp addrs = do
  -- Idea: we generate the expression in reverse order, pushing them onto the stack
  -- or moving them into the appropriate register
  let expAddrs = zip exp (reverse addrs)
  join <$> forM expAddrs (\(e,a) -> do
                    e' <- exp2asm e
                    let save = case a of
                          RegArg i s -> [Mov (SrcReg Rax) (DestReg (argReg !! i))]
                          StackArg i s -> (if s > 8 then [Push Rdx] else []) ++ [Push Rax]
                    return $ e' ++ save)

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
stm2asm (IR.FPro (AA.AsmFunc _ _ _ bdy)) = return [InstrBlock bdy]
stm2asm (IR.FPro f@(AA.Func _ qname _ _ offset _)) = do
  IR.setFunc f
  return [ Lib.Asm.Label (if getName qname == "main" then "main" else show qname)
         , Push Rbp
         , Mov (SrcReg Rsp) (DestReg Rbp)
         , Sub (IInt (-1 * offset)) (DestReg Rsp)
         ]
stm2asm (IR.FEpi (AA.Func _ qname _ _ offset _)) =
  return $ [ Add (IInt (-1 * offset)) (DestReg Rsp)
         , Pop Rbp ] ++
    if getName qname == "main"
    then [
      Mov (SrcReg Rax) (DestReg Rdi)
    , Mov (IInt 60) (DestReg Rax)
    , Syscall
    ] else [Ret]
stm2asm (IR.FEpi AA.AsmFunc{}) = error "Asm funcs don't have epilogues"

delim :: String -> [AInstr] -> [AInstr]
delim blk instrs = Comment blk : instrs ++ [Comment $ "/" ++ blk]
