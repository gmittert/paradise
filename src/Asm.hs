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
  | Eax
  | Ebx
  | Ecx
  | Edx
  | Ebp
  | Esp
  | Edi
  | Esi
  | Ah
  | Al
  | Bh
  | Bl
  | Ch
  | Cl
  | Dh
  | Dl
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
  show Eax = "eax"
  show Ebx = "ebx"
  show Ecx = "ecx"
  show Edx = "edx"
  show Ebp = "ebp"
  show Esp = "esp"
  show Edi = "edi"
  show Esi = "esi"
  show Ah = "ah"
  show Al = "al"
  show Bh = "bh"
  show Bl = "bl"
  show Ch = "ch"
  show Cl = "cl"
  show Dh = "dh"
  show Dl = "dl"

data Src
  = SrcReg Reg
  | IInt Int
  | ISOffset Int
  | SDeref Src
  deriving (Eq)

instance Show Src where
  show (SrcReg a) = "%" ++ show a
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
  | Cmp Src Dest
  | Setl Dest
  | Setle Dest
  | Imul Src
  | Idiv Src
  | Push Reg
  | Pop Reg
  | Jmp String
  | Je String
  | Movsx Dest Dest
  | CQO
  | Leave
  | Ret
  | Syscall
  | Comment String

show1 :: Show a => String -> a -> String
show1 s a = s ++ " " ++ show a ++ "\n"

show2 :: (Show a, Show b) => String -> a -> b -> String
show2 s a b = s ++ " " ++ show a ++ ", " ++ show b ++ "\n"

instance Show AInstr where
  show CQO = "cqo\n"
  show (Setl a) = show1 "setl" a
  show (Setle a) = show1 "setle" a
  show (Cmp a b) = show2 "cmp" a b
  show (Globl a) = show1 ".globl" a
  show (Label a) = a ++ ":\n"
  show (Mov a b) = show2 "movq" a b
  show (Movsx a b) = show2 "movsx" a b
  show (Add a b) = show2 "addq" a b
  show (Sub a b) = show2 "subq" a b
  show (Imul a) = show1 "imulq" a
  show (Idiv a) = show1 "idivq" a
  show (Push a) = "pushq %" ++ show a ++ "\n"
  show (Pop a) = "popq %" ++ show a ++ "\n"
  show (Jmp l) = show1 "jmp" l
  show (Je l) = show1 "je" l
  show Leave = "leave\n"
  show Ret = "ret\n"
  show Syscall = "syscall\n"
  --show (Comment a) = "#" ++ a ++ "\n"
  show (Comment a) = ""

formatAsm :: [AInstr] -> String
formatAsm instrs = foldr (\x y -> show x ++ y) "" instrs
