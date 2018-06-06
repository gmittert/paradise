module Lib.Types where

import Lib.Format

-- |Position
data Posn = Posn
  { line :: Int
  , col :: Int
  } deriving (Eq, Ord, Show)

-- |A Variable can either be an lval or an rval
data VarDir
  = LVal
  | RVal
  deriving (Eq, Ord, Show)

data IntSize
  = IUnspec
  | I1
  | I8
  | I16
  | I32
  | I64
  deriving (Eq, Ord, Show)

data FloatSize
  = FUnspec
  | F32
  | F64
  deriving (Eq, Ord, Show)

data SignType
  = Signed
  | Unsigned
  | SUnspec
  deriving (Eq, Ord, Show)

data Type
  -- |Size Integer types
  = Int { sz :: IntSize
        , signed :: SignType }
  | Float { fsz :: FloatSize }
  | Void
  | Char
  | Str Int
  -- |Type and length
  | Arr Type
        Int
  | List Type
  | F Type
      [Type]
  | Varargs
  | Ptr Type
  | UserType Name
  | TypeVar Int
  deriving (Eq, Ord)

-- | A Type declaration is a type name, then a list of a type constructor and
-- its arguments
--
-- e.g. type Foo = Bar Buzz | Baz would be
-- (TypeDec Foo [(Bar, [Buzz]), (Baz, [])])
data TypeDec = TypeDec {typeName :: Name, args :: [(Name, [Type])]}
  deriving (Eq, Ord, Show)

-- | Get a tag for a constructor
getTag :: Name -> TypeDec -> Int
getTag n (TypeDec _ []) = error $ "Failed to get tag for " ++ show n
getTag n (TypeDec t ((name, _):xs))
  | n == name = 0
  | otherwise = 1 + getTag n (TypeDec t xs)

-- | Const to indicate any allowed array length
arrAnyLen :: Int
arrAnyLen = -1

instance Show Type where
  show (Int I64 Signed) = "i64"
  show (Int I32 Signed) = "i32"
  show (Int I16 Signed) = "i16"
  show (Int I8 Signed) = "i8"
  show (Int I1 Signed) = "i1"
  show (Int I64 Unsigned) = "u64"
  show (Int I32 Unsigned) = "u32"
  show (Int I16 Unsigned) = "u16"
  show (Int I8 Unsigned) = "u8"
  show (Int I1 Unsigned) = "u1"
  show (Int _ _) = "int"
  show (Float F64) = "f64"
  show (Float F32) = "f32"
  show (Float _) = "float"
  show (Str _) = "str"
  show Void = "void"
  show Char = "char"
  show (Arr t len) = "[" ++ show len ++ " x " ++ show t ++ "]"
  show (List t) = "[" ++ show t ++ "]"
  show (F to args) = show args ++ " -> " ++ show to
  show Varargs = "..."
  show (Ptr t) = show t ++ "*"
  show (UserType n) = show n
  show (TypeVar n) = "t" ++ show n

isNumericArr :: Type -> Bool
isNumericArr (Arr t _) = isNumericArr t
isNumericArr t = isNumeric t

isNumeric :: Type -> Bool
isNumeric (Int _ _) = True
isNumeric (Float _) = True
isNumeric (Str _) = False
isNumeric Void = False
isNumeric Char = False
isNumeric Arr {} = False
isNumeric List {} = False
isNumeric F {} = False
isNumeric Varargs = False
isNumeric Ptr {} = False
isNumeric UserType {} = False
isNumeric TypeVar {} = False

isArr :: Type -> Bool
isArr (Arr _ _) = True
isArr _ = False

data Def
  = FuncDef Type
            [Type]
  | VarDef (Maybe Type)   -- ^ Refer to a local definition
  | ParamDef (Maybe Type) -- ^ Refer to a function parameter
  | CDef CFunc
  | QName QualifiedName
  deriving (Eq, Ord, Show)

-- |Binary operations used in kernel
data KBinOp
  = ElemPlus
  | ElemMult
  | MMult
  | KAssign
  deriving (Eq, Ord)

instance Show KBinOp where
  show ElemPlus = ".+"
  show ElemMult = ".*"
  show MMult = "*"
  show KAssign = "="

kopToBop :: KBinOp -> BinOp
kopToBop ElemPlus = Plus
kopToBop ElemMult = Times
kopToBop KAssign = Assign
kopToBop MMult = undefined

-- |Binary operations
data BinOp
  = Plus
  | Minus
  | Times
  | Div
  | Lt
  | Lte
  | Gt
  | Gte
  -- |a[b], will be decided as L/R in the resolver
  | ArrAccess
  -- |a[b], returns an rval
  | ArrAccessR
  -- |a[b] = ..., returns an lval
  | ArrAccessL
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
  show ArrAccess = "@"
  show ArrAccessL = "@L"
  show ArrAccessR = "@R"
  show Assign = "="

data UnOp
  = Len
  | Neg
  | Not
  | Cast Type
  deriving (Eq, Ord)

instance Show UnOp where
  show Len = "#"
  show Neg = "-"
  show Not = "!"
  show (Cast t) = ": " ++ show t

type Size = Int

-- |Returns the internal size of the type, that is, how much space we have to
-- allocate for it
toSize :: Type -> Size
toSize (Int I64 _) = 8
toSize (Int I32 _) = 4
toSize (Int I16 _) = 2
toSize (Int I8 _) = 1
toSize (Int I1 _) = 1
toSize (Float F64) = 8
toSize (Float F32) = 4
toSize Char = 1
toSize (Arr i n) = toSize i * n
toSize a = error $ show a ++ " has no size"

-- |A type a is promotable to a type b if we can safely cast a to b
promotable :: Type -> Type -> Bool
promotable t1 t2 =
  case promote t1 t2 of
    Just _ -> True
    Nothing -> False

promote :: Type -> Type -> Maybe Type
promote (Int I1 Unsigned) (Int I1 Signed) = Nothing
promote (Int I1 Unsigned) (Int sz2 Signed) = Just (Int sz2 Signed)
promote (Int sz1 Signed) (Int sz2 Signed) =
  if sz1 <= sz2
    then Just (Int sz2 Signed)
    else Nothing
promote (Int sz1 SUnspec) (Int sz2 SUnspec) =
  if sz1 <= sz2
    then Just (Int sz2 SUnspec)
    else Nothing
promote (Int _ _) (Float sz) = Just (Float sz)
promote (Int sz1 Unsigned) (Int sz2 Unsigned) =
  if sz1 <= sz2
    then Just (Int sz2 Unsigned)
    else Nothing
promote (Float FUnspec) (Float sz) = Just (Float sz)
promote (Float sz1) (Float sz2) =
  if sz1 <= sz2
    then Just (Float sz2)
    else Nothing
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
newtype Name = Name
  { toString :: String
  } deriving (Eq, Ord)

instance Show Name where
  show = toString

{- A general purpose qualified name
 - e.g. Foo.Bar.Baz
-}
data QualifiedName =
  QualifiedName ModulePath
                Name
  deriving (Eq, Ord)

instance Show QualifiedName where
  show (QualifiedName m n) =
    case show m of
      "" -> show n
      a -> a ++ "_" ++ show n

-- |Make a qualified name out of a path and name
mkQName :: ModulePath -> Name -> QualifiedName
-- Treat main differently so we can identify it later
mkQName _ (Name "main") = QualifiedName (ModulePath []) (Name "main")
mkQName path n = QualifiedName path n

-- |Check if a given qualified name is the main function
isMain :: QualifiedName -> Bool
isMain (QualifiedName (ModulePath []) (Name "main")) = True
isMain _ = False

-- |Given a qualified name, it its corresponding non qualified name
getName :: QualifiedName -> Name
getName (QualifiedName _ n) = n

-- |A module path is a list of string, e.g. src/main/foo/bar.para is
-- ["src", "main", "foo", "bar.para"]
newtype ModulePath =
  ModulePath [String]
  deriving (Eq, Ord)

instance Show ModulePath where
  show (ModulePath []) = ""
  show (ModulePath m) = tail $ concatMap ((:) '_') m

-- |A foreign c function definition
data CFunc = CFunc
  { cname :: Name
  , ctype :: Type
  , cargs :: [Type]
  } deriving (Eq, Ord)

instance Show CFunc where
  show (CFunc n t ts) = show n ++ "(" ++ commaList ts ++ "):" ++ show t

modulePathToFile :: ModulePath -> String
modulePathToFile (ModulePath m) = tail $ concatMap ((:) '/') m ++ ".para"

fileToModulePath :: String -> ModulePath
fileToModulePath f = ModulePath $ parseFile f
  where
    parseFile :: String -> [String]
    parseFile f =
      let (front, back) = span (/= '/') f
       in case back
        -- Drop the para from the last block
                of
            [] -> [(reverse . drop 5 . reverse) front]
            _ -> front : parseFile (tail back)
