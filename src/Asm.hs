module Asm where

data Reg
  = Eax
  | Ebx
  deriving (Eq)

instance Show Reg where
  show Eax = "eax"
  show Ebx = "ebx"

data Src
  = SrcReg Reg
  | Immediate Int
  deriving (Eq)

instance Show Src where
  show (SrcReg a) = "%" ++ show a
  show (Immediate a) = "$" ++ show a

data Dest
  = DestReg Reg
  deriving (Show, Eq)

data Instr
  = Globl String
  | Type String
  | Label String
  | Movl Src Dest
  | Addl Src Dest
  | Ret

instance Show Instr where
  show (Globl a) = ".globl " ++ a ++ "\n"
  show (Type a) = ".type " ++ a ++ ", @function\n"
  show (Label a) = a ++ ":\n"
  show (Movl a b) = "movl " ++ show a ++ ", " ++ show b ++ "\n"
  show (Addl a b) = "addl " ++ show a ++ ", " ++ show b ++ "\n"
  show Ret = "ret\n"

