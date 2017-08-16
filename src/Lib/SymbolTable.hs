module Lib.SymbolTable where

import Types
import Data.Map.Strict as M

data Entry a = Entry Type a
  deriving (Eq, Ord, Show)

data SymbolTable a = SymbolTable {
  vars :: M.Map Name (Entry a)
  , funcs :: M.Map Name (Entry a)
  } deriving (Eq, Ord, Show)

emptyTable :: SymbolTable a
emptyTable = SymbolTable M.empty M.empty
