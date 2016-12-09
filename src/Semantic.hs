module Semantic where

import qualified Data.Map.Strict as M
import Syntax

data Addr
  = AName String
  | Const Int
  | Temp String
  deriving (Eq, Ord, Show)

data Entry = Entry Type Addr Int
  deriving (Eq, Ord, Show)

data SymbolTable = SymbolTable (M.Map Name Entry) Int Int
  deriving (Eq, Ord, Show)

buildSymbolTable :: Prog -> SymbolTable
buildSymbolTable (Prog decls _ _) = declsToMap decls

declsToMap :: Decls -> SymbolTable
declsToMap (Decls' _name _type) = SymbolTable
  (M.singleton _name (Entry _type (AName $ toString _name) 0))
  0
  (toSize _type)
declsToMap (Decls _decls _name _type) =
  let (SymbolTable _map nacc offset) = declsToMap _decls in
    SymbolTable
      (M.insert _name (Entry _type (AName $ toString _name) offset) _map)
      nacc
      (offset + toSize _type)

lookup :: Name -> SymbolTable -> Addr
lookup name (SymbolTable m _ _ ) =
  case M.lookup name m of
    Just (Entry _ addr _) -> addr
    Nothing -> error $ "Could not find " ++ toString name ++ " in table " ++ show m

newTemp :: Type -> SymbolTable -> (Addr, SymbolTable)
newTemp typ (SymbolTable m nacc offset) =
  (Temp (show (nacc + 1)), SymbolTable
    (M.insert (Name (show nacc)) (Entry typ (Temp (show nacc)) offset) m)
    (nacc + 1)
    (offset + toSize typ))
