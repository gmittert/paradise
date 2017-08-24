module Lib.SymbolTable where

import Types
import Data.Map.Strict as M
import Control.Applicative

data SymbolTable = SymbolTable {
  locals :: M.Map Name Def
  , globals :: M.Map Name Def
  } deriving (Eq, Ord)

-- | Look up a name in the symbol table, checking the locals, then falling
-- back on the globals
lookup :: Name -> SymbolTable -> Maybe Def
lookup name table =
  M.lookup name (locals table) <|> M.lookup name (globals table)

addLocal :: Name -> Def -> SymbolTable-> SymbolTable
addLocal name entry scope = scope {
  locals = M.insert name entry (locals scope)
}

addGlobal :: Name -> Def -> SymbolTable -> SymbolTable
addGlobal name entry scope = scope {
  globals = M.insert name entry (globals scope)
}

instance Show SymbolTable where
  show table = let f k _ result = result ++ show k ++ " "
    in M.foldrWithKey f "{ " (locals table) ++ "}"

emptyTable :: SymbolTable
emptyTable = SymbolTable M.empty M.empty
