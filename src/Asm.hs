module Asm where

data Reg
  = Rax
  | Rbx
  | Rcx
  | Rdx
  | Rbp
  | Rsp
  | Rdi
  | Rsi
  deriving (Eq)

instance Show Reg where
  show Rax = "rax"
  show Rbx = "rbx"
  show Rcx = "rcx"
  show Rdx = "rdx"
  show Rbp = "rbp"
  show Rsp = "rsp"
  show Rdi = "rdi"
  show Rsi = "rsi"

data Src
  = SrcReg Reg
  | SrcRegPtr Reg
  | SrcRegPtr' Reg Int
  | IInt Int
  | ISOffset Int
  deriving (Eq)

instance Show Src where
  show (SrcReg a) = "%" ++ show a
  show (SrcRegPtr a) = "(%" ++ show a ++ ")"
  show (SrcRegPtr' a offset) = show (-1*offset) ++ "(%" ++ show a ++ ")"
  show (ISOffset a) = show ((-1) * a) ++ "(%rbp)"
  show (IInt a) = "$" ++ show a

data Dest
  = DestReg Reg
  | IDOffset Int
  deriving (Eq)

instance Show Dest where
  show (DestReg a) = "%" ++ show a
  show (IDOffset a) = "-" ++ show a ++ "(%rbp)"

data AInstr
  = Globl String
  | Label String
  | Mov Src Dest
  | Add Src Dest
  | Sub Src Dest
  | Push Reg
  | Pop Reg
  | Leave
  | Ret
  | Syscall
  | Comment String

instance Show AInstr where
  show (Globl a) = ".globl " ++ a ++ "\n"
  show (Label a) = a ++ ":\n"
  show (Mov a b) = "movq " ++ show a ++ ", " ++ show b ++ "\n"
  show (Add a b) = "addq " ++ show a ++ ", " ++ show b ++ "\n"
  show (Sub a b) = "subq " ++ show a ++ ", " ++ show b ++ "\n"
  show (Push a) = "pushq %" ++ show a ++ "\n"
  show (Pop a) = "popq %" ++ show a ++ "\n"
  show Leave = "leave\n"
  show Ret = "ret\n"
  show Syscall = "syscall\n"
  --show (Comment a) = "#" ++ a ++ "\n"
  show (Comment a) = ""
