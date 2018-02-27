module Lib.SymbolTable where

import Lib.Types
import Data.Map.Strict as M
import Control.Applicative

data SymbolTable = SymbolTable {
  locals :: M.Map Name Def
  , globals :: M.Map Name (QualifiedName, Def)
  } deriving (Eq, Ord)

-- | Look up a name in the symbol table, checking the locals, then falling
-- back on the globals
lookup :: Name -> SymbolTable -> Maybe Def
lookup name (SymbolTable locals globals)=
  M.lookup name locals <|> (snd <$> M.lookup name globals)

lookupName :: Name -> SymbolTable -> Maybe QualifiedName
lookupName name (SymbolTable _ globals)=
  fst <$> M.lookup name globals

addLocal :: Name -> Def -> SymbolTable-> SymbolTable
addLocal name entry scope = scope {
  locals = M.insert name entry (locals scope)
}

addGlobal :: Name -> Def -> QualifiedName -> SymbolTable -> SymbolTable
addGlobal name entry qname scope = scope {
  globals = M.insert name (qname, entry) (globals scope)
}

instance Show SymbolTable where
  show (SymbolTable locals globals) = let
    formatEntry k _ result = result ++ show k ++ "; "
    in (M.foldrWithKey formatEntry "locals: { " locals ++ "}")
    ++ (M.foldrWithKey formatEntry "globals: { " globals ++ "}")

emptyTable :: SymbolTable
emptyTable = SymbolTable M.empty M.empty
