module Compile where

import Parser (parseProg)
import Resolver
import Weeder
import Typer
import Lib.IR
import Addresser
import GenIR
import BasicBlocks
import Canonicalizer
import Codegen
import Control.Monad.State.Lazy

import Lib.Asm

compile :: String -> Either String String
compile input = let
  asm = parseProg input
    >>= weeder
    >>= resolver
    >>= typer
    >>= addresser
    >>= genIR
    >>= canonicalize
    >>= basicBlocks
    >>= codegen
  in case asm of
    Right res -> Right $ formatAsm $ evalState (irgen res) Lib.IR.emptyState
    Left err -> Left err

ir :: String -> Either String String
ir input = let
  asm = parseProg input
    >>= weeder
    >>= resolver
    >>= typer
    >>= addresser
    >>= genIR
    >>= canonicalize
    >>= basicBlocks
  in case asm of
    Right res -> Right $ show $ evalState (irgen res) Lib.IR.emptyState
    Left err -> Left err
