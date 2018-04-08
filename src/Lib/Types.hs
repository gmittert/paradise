module Lib.Types where

-- | A Variable can either be an lval or an rval
data VarDir = LVal| RVal
  deriving (Eq, Ord, Show)

data IntSize = I8 | I16 | I32 | I64
  deriving (Eq, Ord, Show)
data SignType = Signed | Unsigned
  deriving (Eq, Ord, Show)

data Type
  -- | Size Integer types
  = Int {sz :: IntSize, signed :: SignType}
  | Void
  | Bool
  | Char
  | Str
  | Arr Type
-- | Function types
  | F Type [Type]
  deriving (Eq, Ord)

instance Show Type where
  show (Int I64 Signed) = "i64"
  show (Int I32 Signed) = "i32"
  show (Int I16 Signed) = "i16"
  show (Int I8 Signed) = "i8"
  show (Int I64 Unsigned) = "u64"
  show (Int I32 Unsigned) = "u32"
  show (Int I16 Unsigned) = "u16"
  show (Int I8 Unsigned) = "u8"
  show Str = "str"
  show Void = "void"
  show Bool = "bool"
  show Char = "char"
  show (Arr t) = "[" ++ show t ++ "]"
  show (F to args) = show args ++ " -> " ++ show to

isNumeric :: Type -> Bool
isNumeric (Int _ _) = True
isNumeric Str = False
isNumeric Void = False
isNumeric Bool = False
isNumeric Char = False
isNumeric Arr{} = False
isNumeric F{} = False

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

data UnOp = Len | Neg | Not | Alloc
  deriving (Eq, Ord)
instance Show UnOp where
  show Len = "#"
  show Neg = "-"
  show Not = "!"
  show Alloc = "alloc!"

type Size = Int

-- |Returns the internal size of the type, that is, how much space we have to
-- allocate for it
toSize :: Type -> Size
toSize (Int I64 _)= 8
toSize (Int I32 _)= 4
toSize (Int I16 _)= 2
toSize (Int I8 _)= 1
toSize Char = 1
toSize Bool = 1
-- Arrays look like (e.g. 2x2)
-- | dim 2 |                      | arr[11] |
-- | dim 1 | <-- | dim ptr  |     | arr[10] |
--               | num dims |     | arr[01] |
--               | data ptr | --> | arr[00] |
--                   ^
--            arr ---|
toSize (Arr _) = 8
toSize a = error $ show a ++ " has no size"

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
  where
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
  -- In the case of SystemV amd64, the first 48 bytes are passed in registers,
  -- (RDI, RSI, RDX, RCX, R8, R9) and the rest on the stack
  | RegArg {
      -- | Arg is the count'th register argument
      count::Int,
      -- | Size of the argument
      size:: Int}
  | StackArg {
      -- | Arg is the count'th stack argument
      count::Int,
      -- | Size of the argument
      size:: Int}
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
