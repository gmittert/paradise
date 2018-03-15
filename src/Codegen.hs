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
exp2asm (IR.Const i sz) = let sf = sizeToSuffix sz in
  return [Mov sf (IInt i) (DestReg (r 0 sf))]
exp2asm (IR.EName (Lib.Types.Label l) sz) =
  let sf = sizeToSuffix sz in
  return [Mov sf (SLabel l) (DestReg (r 0 sf))]
exp2asm t@(IR.Temp _ sz) = do
  let sf = sizeToSuffix sz
  src <- getVarSrc (IR.tempToName t)
  return [Mov sf src (DestReg (r 0 sf))]
exp2asm (IR.RegArg i sz) =
  let sf = sizeToSuffix sz in
  return [Mov sf (addrToSrc (Lib.Types.RegArg i sz)) (DestReg (r 0 sf))]
exp2asm (IR.StackArg i sz) =
  let sf = sizeToSuffix sz in
  return [Mov sf (addrToSrc (Lib.Types.StackArg i sz)) (DestReg (r 0 sf))]
exp2asm IR.FP = return [Mov Q (SrcReg Rbp) (DestReg (r 0 Q))]
exp2asm (IR.Uop Lib.Types.Neg exp1 sz) = do
  let sf = sizeToSuffix sz
  e1 <- exp2asm exp1
  return $ e1 ++ [Lib.Asm.Neg (SrcReg (r 0 sf))]
exp2asm (IR.Uop Lib.Types.Not exp1 sz) = do
  let sf = sizeToSuffix sz
  e1 <- exp2asm exp1
  return $ e1 ++ [Xor (IInt 1) (DestReg (r 0 sf))]
exp2asm (IR.Uop Alloc exp1 _) = do
  e1 <- exp2asm exp1
  return $ e1 ++ [Call "__alloc"]
exp2asm (IR.Uop Len exp1 sz) = do
  let sf = sizeToSuffix sz
  e1 <- exp2asm exp1
  return $ e1 ++ [
     Mov sf (RegOffset 16 (r 0 sf)) (DestReg (r 0 sf)),
     Mov sf (SDeref (SrcReg (r 0 sf))) (DestReg (r 0 sf))
     ]
exp2asm (IR.Bop op exp1 exp2 sz) = do
  let e1sz = (sizeToSuffix . IR.toSize) exp1
  let e2sz = (sizeToSuffix . IR.toSize) exp2
  let sf = sizeToSuffix sz
  let mkCmp op = [ Cmp (SrcReg (r 1 e2sz)) (SrcReg (r 0 e1sz))
                 , op (DestReg Al)
                 , Movsx (DestReg Al) (DestReg (r 0 Q))]
  e2 <- (++ [Push (r 0 Q)]) <$> exp2asm exp2
  e1 <- exp2asm exp1
  let code = e2 ++ e1 ++ [Pop (r 1 Q)]
  return $ code ++ case op of
    Plus -> [Add sf (SrcReg (r 1 sf)) (DestReg (r 0 sf))]
    Minus -> [Sub sf (SrcReg (r 1 sf)) (DestReg (r 0 sf))]
    Times -> [Imul (SrcReg (r 1 sf))]
    Div -> [ CQO
           , Idiv (SrcReg (r 1 sf))]
    Lt -> mkCmp Setl
    Lte -> mkCmp Setle
    Gt -> mkCmp Setg
    Gte -> mkCmp Setge
    Eq -> mkCmp Sete
    Neq -> mkCmp Setne
    Access -> [Mov sf (SOffset 0 (r 0 Q) (r 1 Q) sz) (DestReg (r 0 sf))]
exp2asm (IR.Mem (IR.Bop Plus IR.FP (IR.Const c sz1) sz2) sz3) =
  let sf = sizeToSuffix sz3 in
  return [Mov sf (ISOffset c) (DestReg (r 0 sf))]
exp2asm (IR.Mem exp sz) = do
  let sf = sizeToSuffix sz
  e <- exp2asm exp
  return $ e ++ [Mov sf (SDeref (SrcReg (r 0 Q))) (DestReg (r 0 sf))]
exp2asm (IR.Call (IR.EName (Lib.Types.Label l) sz1) args addrs sz2) = do
  genArgs <- pushReg args addrs
  return $ saveReg ++ genArgs ++ [Lib.Asm.Call l] ++ restoreReg

exp2asm (IR.ACall (IR.EName (Lib.Types.Label l) sz1) args addrs sz2) = do
  genArgs <- pushReg args addrs
  return $ saveReg ++ genArgs ++ [Lib.Asm.Call l] ++ restoreReg
exp2asm (IR.Eseq s e sz) = do
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
                          RegArg i s -> [Mov (sizeToSuffix s) (SrcReg (r 0 (sizeToSuffix s))) (DestReg (argReg !! i))]
                          StackArg i s -> (if s > 8 then [Push Rdx] else []) ++ [Push (r 0 Q)]
                    return $ e' ++ save)

stm2asm :: IR.Stm -> IR.IRGen [AInstr]
stm2asm (IR.Move (IR.Bop Plus IR.FP (IR.Const c1 sz1) sz2) (IR.Const c2 sz3)) =
  let sf = sizeToSuffix sz2 in
  return [Mov sf (IInt c2) (IDOffset c1)]

stm2asm (IR.Move (IR.Bop Plus IR.FP (IR.Const c sz1) sz2) e2) = do
  let sf = sizeToSuffix sz2
  exp2 <- exp2asm e2
  return $ exp2 ++ [Mov sf (SrcReg (r 0 sf)) (IDOffset c)]
stm2asm (IR.Move t@(IR.Temp _ sz) e2) = do
  let sf = sizeToSuffix sz
  e2' <- exp2asm e2
  dest <- getVarDest (IR.tempToName t)
  return $ e2' ++ [Mov sf (SrcReg (r 0 sf)) dest]
stm2asm (IR.Move e1 e2) = do
  let e1s = sizeToSuffix (IR.toSize e1)
  let e2s = sizeToSuffix (IR.toSize e2)
  exp1 <- exp2asm e1
  exp2 <- exp2asm e2
  return $ exp1 ++ [Push (r 0 Q)] ++ exp2 ++ [Pop (r 1 Q)] ++ [Mov e2s (SrcReg (r 0 e2s)) (DDeref (DestReg (r 1 e1s)))]
stm2asm (IR.Sexp e) = exp2asm e
stm2asm (IR.Ret e) = exp2asm e
stm2asm (IR.Jump e _) = case e of
  IR.JLab (Lib.Types.Label l) -> return [Jmp (SLabel l)]
  IR.Computed e -> (++ [Jmp (SrcReg (r 0 Q))]) <$>  exp2asm e
stm2asm (IR.Cjump op left right (Lib.Types.Label iftrue) (Lib.Types.Label iffalse)) = do
  let sf = sizeToSuffix (IR.toSize right)
  right <- (++ [Push (r 0 Q)]) <$> exp2asm right
  left <- (++ [Pop (r 1 Q)]) <$> exp2asm left
  return $ right ++ left ++ [Cmp (SrcReg (r 0 sf))(SrcReg (r 1 sf)), (case op of
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
         , Mov Q (SrcReg Rsp) (DestReg Rbp)
         , Sub Q (IInt (-1 * offset)) (DestReg Rsp)
         ]
stm2asm (IR.FEpi (AA.Func _ qname _ _ offset _)) =
  return $ [ Add Q (IInt (-1 * offset)) (DestReg Rsp)
         , Pop Rbp ] ++
    if getName qname == "main"
    then [
      Mov Q (SrcReg (r 0 Q)) (DestReg Rdi)
    , Mov Q (IInt 60) (DestReg (r 0 Q))
    , Syscall
    ] else [Ret]
stm2asm (IR.FEpi AA.AsmFunc{}) = error "Asm funcs don't have epilogues"

delim :: String -> [AInstr] -> [AInstr]
delim blk instrs = Comment blk : instrs ++ [Comment $ "/" ++ blk]
