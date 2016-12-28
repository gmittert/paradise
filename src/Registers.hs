{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Registers where

import Optimize
import TAC
import Syntax
import Semantic
import Control.Monad.State.Lazy

data RegState = RegState {
    eax :: Name
  , ebx :: Name
  , ecx :: Name
  }

data Reg = EAX | EBX | ECX

newtype Registers a = Registers { getRegisters :: State RegState a }
  deriving (Functor, Applicative, Monad, MonadState RegState)

data RInstrs
  = BRInstr BinOp Reg Reg
  | URInstr UniOp Reg
  deriving (Eq, Ord, Show)

codeGen :: FlowGraph -> [Instr]
codeGen (FlowGraph blk succ) = genBlock blk ++ concatMap codeGen succ

genBlock :: [Instr] -> [Instr]
genBlock = undefined

genCode :: Instr -> Registers [Instr]
genCode inst = do
  ((reg1, reg2), strInst) <- getReg inst
  return $ strInst ++ [(Addl (SrcReg reg1) (DestReg reg2))]

getOffset :: Name -> Dest
getOffset = undefined

getName :: Addr -> Name
getName = undefined

getReg :: Instr -> Registers ((Reg, Reg), [AInstr])
getReg (BInstr op a1 a2 _) = Registers . state $ \s ->
  (([Eax, Ebx], [Movl (SrcReg Eax) (getOffset (eax s))
                    , Movl (SrcReg Ebx) (getOffset (ebx s))])
  , s{eax = getName a1, ebx = getName a2})
getReg (UInstr op a1 a2) = undefined
