module Lib.SymbolTable where

import Control.Applicative

import qualified Data.Map.Strict as M
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

-- | Given a name, get its type
getType :: Name -> SymbolTable -> Maybe Type
getType n t =
  case Lib.SymbolTable.lookup n t of
    Just (VarDef t) -> t
    Just (ParamDef t) -> t
    Just a -> error $ "Don't know how to get type of " ++ show a
    _ -> error $ "Couldn't get type of " ++ show n ++ " in " ++ show t

-- | Look up a name in the symbol table, checking the locals, then falling
-- back on the globals
lookup :: Name -> SymbolTable -> Maybe Def
lookup name (SymbolTable locals globals _) =
  locals M.!? name <|> (snd <$> globals M.!? name)

-- | Given a name, lookup its fully qualified version
lookupName :: Name -> SymbolTable -> Maybe QualifiedName
lookupName name (SymbolTable _ globals _) = fst <$> globals M.!? name

-- | Add a local to the symbol table
addLocal :: Name -> Def -> SymbolTable -> SymbolTable
addLocal name entry scope = scope {locals = M.insert name entry (locals scope)}

-- | Add a global to the symbol table
addGlobal :: Name -> Def -> QualifiedName -> SymbolTable -> SymbolTable
addGlobal name entry qname scope =
  scope {globals = M.insert name (qname, entry) (globals scope)}

-- | Add a type declaration to the symbol table
addType :: Name -> TypeDec -> SymbolTable -> SymbolTable
addType name entry table = table {types = M.insert name entry (types table)}

-- | Given a type name, get its declaration
lookupType :: Name -> SymbolTable -> Maybe TypeDec
lookupType name (SymbolTable _ _ types) = types M.!? name

-- | Given a type constructor, get the type declaration
lookupTypeCtor :: Name -> SymbolTable -> Maybe TypeDec
lookupTypeCtor name (SymbolTable _ _ types) = let
  getCtors (TypeDec _ args) = map fst args
  hasCtor = elem name . getCtors
  filtered = M.filter hasCtor types
  in case M.toList filtered of
    [] -> Nothing
    ((_,dec):_) -> Just dec

-- | Given a type constructor, get the type declaration
lookupTypeCtorArgs :: Name -> SymbolTable -> Maybe [Type]
lookupTypeCtorArgs name table =
  case lookupTypeCtor name table of
    Nothing -> Nothing
    Just (TypeDec _ args) ->
      let nargs = filter ((== name) . fst) args
      in case nargs of
        [] -> Nothing
        ((_,args):_) -> Just args

-- | Create an empty table
emptyTable :: SymbolTable
emptyTable = mempty
