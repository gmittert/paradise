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
  | SDeref Src
  deriving (Eq)

instance Show Src where
  show (SrcReg a) = "%" ++ show a
  show (SrcRegPtr a) = "(%" ++ show a ++ ")"
  show (SrcRegPtr' a offset) = show (-1*offset) ++ "(%" ++ show a ++ ")"
  show (ISOffset a) = show ((-1) * a) ++ "(%rbp)"
  show (IInt a) = "$" ++ show a
  show (SDeref s) = "(" ++ show s ++ ")"

data Dest
  = DestReg Reg
  | IDOffset Int
  | DDeref Src
  deriving (Eq)

instance Show Dest where
  show (DestReg a) = "%" ++ show a
  show (IDOffset a) = "-" ++ show a ++ "(%rbp)"
  show (DDeref s) = "(" ++ show s ++ ")"

data AInstr
  = Globl String
  | Label String
  | Mov Src Dest
  | Add Src Dest
  | Sub Src Dest
  | Imul Src
  | Idiv Src
  | Push Reg
  | Pop Reg
  | Jmp String
  | Jz String
  | CQO
  | Leave
  | Ret
  | Syscall
  | Comment String

instance Show AInstr where
  show (CQO) = "cqo"++ "\n"
  show (Globl a) = ".globl " ++ a ++ "\n"
  show (Label a) = a ++ ":\n"
  show (Mov a b) = "movq " ++ show a ++ ", " ++ show b ++ "\n"
  show (Add a b) = "addq " ++ show a ++ ", " ++ show b ++ "\n"
  show (Sub a b) = "subq " ++ show a ++ ", " ++ show b ++ "\n"
  show (Imul a) = "imulq " ++ show a ++ "\n"
  show (Idiv a) = "idivq " ++ show a ++ "\n"
  show (Push a) = "pushq %" ++ show a ++ "\n"
  show (Pop a) = "popq %" ++ show a ++ "\n"
  show (Jmp s) = "jmp " ++ show s ++ "\n"
  show (Jz s) = "jz " ++ show s ++ "\n"
  show Leave = "leave\n"
  show Ret = "ret\n"
  show Syscall = "syscall\n"
  --show (Comment a) = "#" ++ a ++ "\n"
  show (Comment a) = ""

formatAsm :: [AInstr] -> String
formatAsm instrs = foldr (\x y -> show x ++ y) "" instrs
