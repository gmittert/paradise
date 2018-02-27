module Lib.Types where

-- | A Variable can either be an lval or an rval
data VarDir = LVal| RVal
  deriving (Eq, Ord, Show)

data Type
  = Int
  | Void
  | Bool
  | Char
  | Pointer Type
  | Arr Type Int
  | F Type [Type]
  deriving (Eq, Ord)
instance Show Type where
  show Int = "int"
  show Void = "void"
  show Bool = "bool"
  show Char = "char"
  show (Pointer t) = "*" ++ show t
  show (Arr t i) = show t ++ "[" ++ show i ++ "]"
  show (F to args) = show args ++ " -> " ++ show to

data Def
  = FuncDef Type [Type]
  | VarDef Type
  | QName QualifiedName
  deriving (Eq, Ord, Show)

data BinOp
  = Plus
  | Minus
  | Times
  | Div
  | Lt
  | Lte
  | Gt
  | Gte
  | Access
  | Eq
  | Neq
  deriving (Eq, Ord)
instance Show BinOp where
  show Plus = "+"
  show Minus = "-"
  show Times = "*"
  show Div = "/"
  show Lt = "<"
  show Lte = "<="
  show Gt = ">"
  show Gte = ">="
  show Neq = "!="
  show Eq = "=="
  show Access = "@"

data UnOp = Deref | Neg | Not
  deriving (Eq, Ord)
instance Show UnOp where
  show Deref = "*"
  show Neg = "-"
  show Not = "!"

toSize :: Type -> Int
toSize Int = 8
toSize (Pointer _) = 8
toSize Char = 1
toSize Void = 0
toSize Bool = 1
toSize (Arr tpe size) = size * toSize tpe
toSize (F _ _) = 8

{- A general purpose unqualified name
 - e.g. foo, bar
 -}
newtype Name = Name {toString :: String}
  deriving (Eq, Ord)
instance Show Name where
  show = toString

{- A general purpose qualified name
 - e.g. Foo.Bar.Baz
-}
data QualifiedName = QualifiedName ModulePath Name
  deriving (Eq, Ord)
instance Show QualifiedName where
  show (QualifiedName m n) = case show m of
    "" -> show n
    a -> a ++ "_" ++ show n

mkQName :: ModulePath -> Name -> QualifiedName
mkQName = QualifiedName

getName :: QualifiedName -> String
getName (QualifiedName _ n) = show n

newtype ModulePath = ModulePath [String]
  deriving (Eq, Ord)
instance Show ModulePath where
  show (ModulePath m) = tail $ concatMap ((:) '_') m

modulePathToFile :: ModulePath -> String
modulePathToFile (ModulePath m) = tail $ concatMap ((:) '/') m ++ ".al"

fileToModulePath :: String -> ModulePath
fileToModulePath f = ModulePath $ parseFile f

parseFile :: String -> [String]
parseFile f = let (front,back) = span (/= '/') f in
  case back of
    -- Drop the .al from the last block
    [] -> [(reverse . drop 3 .reverse) front]
    _ -> front : parseFile (tail back)

data Address
  -- | Globals and functions are addressed by labels
  = Fixed QualifiedName
  -- | Locals are addressed by an offset from the base pointer
  | Offset Int
  -- | Function arguments are given an argument count. This is later turned
  -- into either a register or offset from the base pointer depending on
  -- the number of arguments and calling conventions
  -- In the case of SystemV amd64, the first 6 args are passed in registers,
  -- (RDI, RSI, RDX, RCX, R8, R9) and the rest on the stack
  | Arg Int
  deriving (Eq, Ord, Show)

newtype Label = Label{label :: String}
  deriving (Eq, Ord)
instance Show Label where
  show = label

-- | Get the Label indicating the beginning the prologue of a function
funcBegin :: QualifiedName -> Label
funcBegin qname = Label ("func__" ++ show qname)

-- | Get the Label indicating the beginning of the epilogue of a function
funcEnd :: QualifiedName -> Label
funcEnd qname = Label (show qname ++ "__end")
