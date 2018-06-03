module Lib.SymbolTable where

import Control.Applicative

import Data.Map.Strict as M
import Data.Monoid
import Lib.Types

data SymbolTable = SymbolTable
  { locals :: M.Map Name Def
  , globals :: M.Map Name (QualifiedName, Def)
  , types :: M.Map Name TypeDec
  } deriving (Eq, Ord)

instance Monoid SymbolTable where
  t1 `mappend` t2 =
    SymbolTable
      (locals t1 <> locals t2)
      (globals t1 <> globals t2)
      (types t1 <> types t2)
  mempty = SymbolTable M.empty M.empty M.empty

instance Show SymbolTable where
  show (SymbolTable locals globals types) =
    let formatEntry k v result = result ++ show k ++ ": " ++ show v ++ "; "
     in (M.foldrWithKey formatEntry "locals: { " locals ++ "}\n") ++
        (M.foldrWithKey formatEntry "globals: { " globals ++ "}") ++
        (M.foldrWithKey formatEntry "types: { " types ++ "}")

getType :: Name -> SymbolTable -> Type
getType n t =
  case Lib.SymbolTable.lookup n t of
    Just (VarDef t) -> t
    _ -> error $ "Couldn't get type of " ++ show n ++ " in " ++ show t

-- | Look up a name in the symbol table, checking the locals, then falling
-- back on the globals
lookup :: Name -> SymbolTable -> Maybe Def
lookup name (SymbolTable locals globals _) =
  locals M.!? name <|> (snd <$> globals M.!? name)

lookupName :: Name -> SymbolTable -> Maybe QualifiedName
lookupName name (SymbolTable _ globals _) = fst <$> globals M.!? name

addLocal :: Name -> Def -> SymbolTable -> SymbolTable
addLocal name entry scope = scope {locals = M.insert name entry (locals scope)}

addGlobal :: Name -> Def -> QualifiedName -> SymbolTable -> SymbolTable
addGlobal name entry qname scope =
  scope {globals = M.insert name (qname, entry) (globals scope)}

addType :: Name -> TypeDec -> SymbolTable -> SymbolTable
addType name entry table = table {types = M.insert name entry (types table)}

lookupType :: Name -> SymbolTable -> Maybe TypeDec
lookupType name (SymbolTable _ _ types) = types M.!? name

emptyTable :: SymbolTable
emptyTable = mempty
