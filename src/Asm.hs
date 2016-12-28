module Asm where

data Reg
  = Eax
  | Ebx
  | Ecx
  deriving (Eq)

instance Show Reg where
  show Eax = "eax"
  show Ebx = "ebx"
  show Ecx = "ecx"

data Src
  = SrcReg Reg
  | IInt Int
  | ISOffset Int
  deriving (Eq)

instance Show Src where
  show (SrcReg a) = "%" ++ show a
  show (ISOffset a) = "-" ++ show (4*a) ++ "(%ebp)"
  show (IInt a) = "$" ++ show a

data Dest
  = DestReg Reg
  | IDOffset Int
  deriving (Eq)

instance Show Dest where
  show (DestReg a) = "%" ++ show a
  show (IDOffset a) = "-" ++ show (4*a) ++ "(%ebp)"

data AInstr
  = Globl String
  | Type String
  | Label String
  | Movl Src Dest
  | Addl Src Dest
  | Subl Src Dest
  | Ret

instance Show AInstr where
  show (Globl a) = ".globl " ++ a ++ "\n"
  show (Type a) = ".type " ++ a ++ ", @function\n"
  show (Label a) = a ++ ":\n"
  show (Movl a b) = "mov " ++ show a ++ ", " ++ show b ++ "\n"
  show (Addl a b) = "add " ++ show a ++ ", " ++ show b ++ "\n"
  show (Subl a b) = "sub " ++ show a ++ ", " ++ show b ++ "\n"
  show Ret = "ret\n"
