module Types where
import qualified Data.Map.Strict as M

data Type
  = Int
  | Bool
  | String Int
  | Char
  | Pointer Type
  deriving (Eq, Ord, Show)

toSize :: Type -> Int
toSize Int = 8
toSize (Pointer _) = 8
toSize Bool = 1
toSize (String a) = a + 16
toSize Char = 1

newtype Name = Name {toString :: String}
  deriving (Eq, Ord)

instance Show Name where
  show = toString

data Entry = Entry Type Addr
  deriving (Eq, Ord, Show)

data SymbolTable = SymbolTable {
  vars :: M.Map Name Entry
  , funcs :: M.Map Name Entry
  } deriving (Eq, Ord, Show)

emptyTable :: SymbolTable
emptyTable = SymbolTable M.empty M.empty

addVar :: Name -> Entry -> SymbolTable -> SymbolTable
addVar name entry scope = scope {
  vars = M.insert name entry (vars scope)
}

addFunc :: Name -> Entry -> SymbolTable -> SymbolTable
addFunc name entry scope = scope {
  funcs = M.insert name entry (funcs scope)
}

{-
An address in the symbol table can be the address of a name
or it could be a constant
-}
data Addr
  = Addr Int
  | IInt Int
  | RelPtr Int
  | IBool Bool
  | IChar Char
  deriving (Eq, Ord, Show)

type Size = Int
