module Semantic where

import qualified Data.Map.Strict as M
import Syntax
import Types

{-
An address in the symbol table can be the address of a name
or it could be a constant
-}
data Addr
  = Addr Int
  | Val Int
  | RelPtr Int
  | Bool Bool
  | Char Char
  deriving (Eq, Ord, Show)

type Size = Int

data Entry = Entry Type Addr
  deriving (Eq, Ord, Show)

type SymbolTable = (M.Map Name Entry)

buildSymbolTable :: Prog -> (SymbolTable, Int)
buildSymbolTable (Prog stmnts _) = declsToMap stmnts

declsToMap :: Statements -> (SymbolTable, Int)
declsToMap (Statements' stmnt) = stmntToEntry stmnt M.empty 0
declsToMap (Statements stmnts stmnt) =
  let (table, offset) = declsToMap stmnts in
    stmntToEntry stmnt table offset

stmntToEntry :: Statement -> SymbolTable -> Int -> (SymbolTable, Int)
stmntToEntry (SDecl name typ) table offset = (M.insert name (Entry typ (Addr offset)) table, offset + toSize typ)
stmntToEntry (SDeclAssign name typ _) table offset = stmntToEntry (SDecl name typ) table offset
stmntToEntry _ table offset = (table, offset)
