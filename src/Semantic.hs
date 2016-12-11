module Semantic where

import qualified Data.Map.Strict as M
import Syntax

{-
An address in the symbol table can be the address of a name
or it could be a constant
-}
data Addr
  = Addr Int
  | Val Int
  deriving (Eq, Ord, Show)
type Size = Int

data Entry = Entry Type Addr
  deriving (Eq, Ord, Show)

type SymbolTable = (M.Map Name Entry)

buildSymbolTable :: Prog -> (SymbolTable, Int)
buildSymbolTable (Prog decls _ _) = declsToMap decls

declsToMap :: Decls -> (SymbolTable, Int)
declsToMap (Decls' name typ) = (M.singleton name (Entry typ (Addr 0)), 0)
declsToMap (Decls decls name typ) =
  let (table, offset) = declsToMap decls in
    (M.insert name (Entry typ (Addr offset)) table, offset + toSize typ)
