module Lib.Asm where
import qualified Lib.Types as Types

data Reg
  = Rax | Rbx | Rcx | Rdx | Rbp | Rsp | Rdi | Rsi | R8 | R9
  | Eax | Ebx | Ecx | Edx | Ebp | Esp | Edi | Esi
  | Ax  | Bx  | Cx  | Dx  | Sp  | Bp  | Si  | Di
  | Ah  | Al  | Bh  | Bl  | Ch  | Cl  | Dh  | Dl
  deriving (Eq)

data Suffix = Q | L | W | B
  deriving (Eq, Ord)
instance Show Suffix where
  show Q = "q"
  show L = "l"
  show W = "w"
  show B = "b"

r :: Int -> Suffix -> Reg
r i sz = let
  quads = [ Rax, Rbx, Rcx, Rdx, Rdi, Rsi, R8, R9]
  longs = [ Eax, Ebx, Ecx, Edx, Edi, Esi]
  words = [ Ax, Bx, Cx, Dx, Si, Di]
  bytes = [ Ah, Al, Bh, Bl, Ch, Cl, Dh, Dl] in
  case sz of
    Q -> quads !! i
    L -> longs !! i
    W -> words !! i
    B -> bytes !! i

sizeToSuffix :: Types.Size -> Suffix
sizeToSuffix 8 = Q
sizeToSuffix 4 = L
sizeToSuffix 2 = W
sizeToSuffix 1 = B
sizeToSuffix a = error $ "No matching suffix for size: " ++ show a


instance Show Reg where
  show Rax = "rax"
  show Rbx = "rbx"
  show Rcx = "rcx"
  show Rdx = "rdx"
  show Rbp = "rbp"
  show Rsp = "rsp"
  show Rdi = "rdi"
  show Rsi = "rsi"
  show R8 = "r8"
  show R9 = "r9"
  show Eax = "eax"
  show Ebx = "ebx"
  show Ecx = "ecx"
  show Edx = "edx"
  show Ebp = "ebp"
  show Esp = "esp"
  show Edi = "edi"
  show Esi = "esi"
  show Ax = "ax"
  show Bx = "bx"
  show Cx = "cx"
  show Dx = "dx"
  show Si = "si"
  show Di = "di"
  show Bp = "bp"
  show Sp = "sp"
  show Ah = "ah"
  show Al = "al"
  show Bh = "bh"
  show Bl = "bl"
  show Ch = "ch"
  show Cl = "cl"
  show Dh = "dh"
  show Dl = "dl"

data Src
  -- | A Source Register
  = SrcReg Reg
  -- | An immediate integer
  | IInt Int
  -- | An offset on the stack from the base pointer
  | ISOffset Int
  -- | An arbitrary offset 
  | SOffset Int Reg Reg Int
  -- | An offset from a register
  | RegOffset Int Reg
  -- | A dereference
  | SDeref Src
  -- | A label
  | SLabel String
  deriving (Eq)

instance Show Src where
  show (SrcReg a) = "%" ++ show a
  show (ISOffset a) = show a ++ "(%rbp)"
  show (SOffset off base rmult imult) = show off ++ "(%" ++ show base ++ ", %" ++ show rmult ++ ", " ++ show imult ++ ")"
  show (RegOffset off base) = show off ++ "(%" ++ show base ++ ")"
  show (IInt a) = "$" ++ show a
  show (SDeref s) = "(" ++ show s ++ ")"
  show (SLabel l) = show l

data Dest
  = DestReg Reg
  | IDOffset Int
  | IDROffset Int Reg
  | DOffset Int Reg Reg Int
  | DDeref Dest
  | DLabel String
  deriving (Eq)

instance Show Dest where
  show (DestReg a) = "%" ++ show a
  show (IDOffset a) = show a ++ "(%rbp)"
  show (IDROffset a r) = show a ++ "("++ show r ++ ")"
  show (DDeref s) = "(" ++ show s ++ ")"
  show (DOffset off base rmult imult) = show off ++ "(%" ++ show base ++ ", %" ++ show rmult ++ ", " ++ show imult ++ ")\n"
  show (DLabel l) = show l

data AInstr
  = Globl String
  | Extern String
  | Label String
  | Mov Suffix Src Dest
  | Add Suffix Src Dest
  | Sub Suffix Src Dest
  | Cmp Src Src
  | Setl Dest
  | Setle Dest
  | Setg Dest
  | Setge Dest
  | Sete Dest
  | Setne Dest
  | Imul Src
  | AInt Int
  | Idiv Src
  | Push Reg
  | Pop Reg
  | Jmp Src
  | Jl Src
  | Jle Src
  | Jg Src
  | Jge Src
  | Je Src
  | Jne Src
  | Movsx Dest Dest
  | Neg Src
  | CQO
  | Leave
  | Call String
  | Ret
  | Syscall
  | Comment String
  | Xor Src Dest
-- | A literal block of asm to be inserted
  | InstrBlock String

show1 :: Show a => String -> a -> String
show1 s a = s ++ " " ++ show a ++ "\n"

show2 :: (Show a, Show b) => String -> a -> b -> String
show2 s a b = s ++ " " ++ show a ++ ", " ++ show b ++ "\n"

instance Show AInstr where
  show CQO = "cqo\n"
  show (Setl a) = show1 "setl" a
  show (Setle a) = show1 "setle" a
  show (Setg a) = show1 "setg" a
  show (Setge a) = show1 "setge" a
  show (Sete a) = show1 "sete" a
  show (Setne a) = show1 "setne" a
  show (Cmp a b) = show2 "cmp" a b
  show (Globl a) = show1 ".globl" a
  show (Extern a) = ".extern " ++ a ++ "\n"
  show (Label a) = (if a == "main" then ".globl _start\n_start:\n" else "") ++ a ++ ":\n"
  show (Mov s a b) = show2 ("mov" ++ show s) a b
  show (Movsx a b) = show2 "movsx" a b
  show (Add s a b) = show2 ("add" ++ show s) a b
  show (Sub s a b) = show2 ("sub" ++ show s) a b
  show (Neg a) = show1 "neg" a
  show (Imul a) = show1 "imulq" a
  show (AInt a) = "int $" ++ show a
  show (Idiv a) = show1 "idivq" a
  show (Push a) = "push %" ++ show a ++ "\n"
  show (Pop a) = "pop %" ++ show a ++ "\n"
  show (Jmp l) = case l of
    SrcReg a -> "jmp *%" ++ show a ++"\n"
    _ -> show1 "jmp" l
  show (Je l) = show1 "je" l
  show (Jne l) = show1 "jne" l
  show (Jg l) = show1 "jg" l
  show (Jge l) = show1 "jge" l
  show (Jl l) = show1 "jl" l
  show (Jle l) = show1 "jle" l
  show (Call l) = show1 "call" l
  show Leave = "leave\n"
  show Ret = "ret\n"
  show Syscall = "syscall\n"
  show (Comment a) = "# " ++ a ++ "\n"
  show (Xor s d) = show2 "xorq" s d
  show (InstrBlock b) = b ++ "\n"

formatAsm :: [AInstr] -> String
formatAsm = foldr (\x y -> show x ++ y) ""

addrToSrc :: Types.Address -> Src
addrToSrc (Types.StackArg addr _) = ISOffset addr
addrToSrc (Types.RegArg addr _)
   | addr <= 6 = SrcReg $ [Rdi, Rsi, Rdx, Rcx, R8, R9] !! addr
   | otherwise = error "Allocated more than 6 registers for fparams"
addrToSrc (Types.Offset a) = ISOffset a
addrToSrc (Types.Fixed n) = SLabel (show n)

addrToDest :: Types.Address -> Dest
addrToDest (Types.StackArg addr _) = IDOffset addr
addrToDest (Types.RegArg addr _)
   | addr <= 6 = DestReg $ [Rdi, Rsi, Rdx, Rcx, R8, R9] !! addr
   | otherwise = error "Allocated more than 6 registers for fparams"
addrToDest (Types.Offset a) = IDOffset a
addrToDest (Types.Fixed n) = DLabel (show n)
