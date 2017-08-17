module Lib.SymbolTable where

import Types
import Data.Map.Strict as M

data Entry a = Entry Type a
  deriving (Eq, Ord, Show)

data SymbolTable a = SymbolTable {
  vars :: M.Map Name (Entry a)
  , funcs :: M.Map Name (Entry a)
  } deriving (Eq, Ord)
instance Show (SymbolTable a) where
  show table = let f k _ result = result ++ show k ++ " "
    in M.foldrWithKey f "{ " (vars table) ++ "}"

emptyTable :: SymbolTable a
emptyTable = SymbolTable M.empty M.empty
