module Lib.Types where

-- | A Variable can either be an lval or an rval
data VarDir = LVal| RVal
  deriving (Eq, Ord, Show)

data IntSize = IUnspec | I8 | I16 | I32 | I64
  deriving (Eq, Ord, Show)
data FloatSize = FUnspec | F32 | F64
  deriving (Eq, Ord, Show)
data SignType = Signed | Unsigned | SUnspec
  deriving (Eq, Ord, Show)

data Type
  -- | Size Integer types
  = Int {sz :: IntSize, signed :: SignType}
  | Float {fsz :: FloatSize}
  | Void
  | Bool
  | Char
  | Str
  -- | No type specified, needs to be inferred
  | TUnspec
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
  show (Int _ _) = "int"
  show (Float F64) = "f64"
  show (Float F32) = "f32"
  show (Float _) = "float"
  show Str = "str"
  show Void = "void"
  show Bool = "bool"
  show Char = "char"
  show TUnspec = "*"
  show (Arr t) = "[" ++ show t ++ "]"
  show (F to args) = show args ++ " -> " ++ show to

isNumeric :: Type -> Bool
isNumeric (Int _ _) = True
isNumeric (Float _) = True
isNumeric TUnspec = True
isNumeric Str = False
isNumeric Void = False
isNumeric Bool = False
isNumeric Char = False
isNumeric Arr{} = False
isNumeric F{} = False

isArr :: Type -> Bool
isArr (Arr _) = True
isArr _ = False

data Def
  = FuncDef Type [Type]
  | VarDef Type
  | QName QualifiedName
  deriving (Eq, Ord, Show)

-- | Binary operations used in kernel
data KBinOp
  = ElemPlus
  | ElemMult
  | MMult
  | KAssign
  deriving (Eq, Ord)
instance Show KBinOp where
  show ElemPlus = ".+"
  show ElemMult= ".*"
  show MMult = "*"
  show KAssign = "="

kopToBop :: KBinOp -> BinOp
kopToBop ElemPlus = Plus
kopToBop ElemMult = Times
kopToBop KAssign = Assign
kopToBop MMult = undefined

-- | Binary operations
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
  | Assign
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
  show Assign = "="

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

promotable :: Type -> Type -> Bool
promotable t1 t2 = case promote t1 t2 of
  Just _ -> True
  Nothing -> False

promote :: Type -> Type -> Maybe Type
promote (Int sz1 Signed) (Int sz2 Signed) = if sz1 <= sz2 then Just (Int sz2 Signed) else Nothing
promote (Int _ _) (Float sz) = Just (Float sz)
promote (Int sz1 Unsigned) (Int sz2 Unsigned) = if sz1 <= sz2 then Just (Int sz2 Unsigned) else Nothing
promote (Float FUnspec) (Float sz) = Just (Float sz)
promote (Float sz1) (Float sz2) = if sz1 <= sz2 then Just (Float sz2) else Nothing
promote (Int IUnspec s1) (Int sz s2) = promote (Int sz s1) (Int sz s2)
promote (Int sz1 SUnspec) (Int sz2 s) = promote (Int sz1 s) (Int sz2 s)
promote _ _ = Nothing

comparable :: Type -> Type -> Bool
comparable (Int _ _) (Int _ _) = True
comparable (Int _ _) (Float _) = True
comparable (Float _) (Int _ _) = True
comparable (Float _) (Float _) = True
comparable a b = a == b

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
mkQName _ (Name "main")= QualifiedName (ModulePath []) (Name "main")
mkQName path n = QualifiedName path n

getName :: QualifiedName -> String
getName (QualifiedName _ n) = show n

newtype ModulePath = ModulePath [String]
  deriving (Eq, Ord)
instance Show ModulePath where
  show (ModulePath []) = ""
  show (ModulePath m) = tail $ concatMap ((:) '_') m

modulePathToFile :: ModulePath -> String
modulePathToFile (ModulePath m) = tail $ concatMap ((:) '/') m ++ ".para"

fileToModulePath :: String -> ModulePath
fileToModulePath f = ModulePath $ parseFile f
  where
    parseFile :: String -> [String]
    parseFile f = let (front,back) = span (/= '/') f in
      case back of
        -- Drop the para from the last block
        [] -> [(reverse . drop 5 .reverse) front]
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
