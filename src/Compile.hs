module Compile where

import Parser (parseProg)
import Lib.Asm
import Resolver
import Weeder
import Typer
import Grapher
import Lib.IR
import Lib.Graph
import Addresser
import GenIR
import Codegen
import BasicBlocks
import Ast.TypedAst
import Ast.ResolvedAst

process :: String -> Either String String
process input = let
  asm = parseProg input
    >>= weeder
    >>= resolver
    >>= typer
    >>= addresser
    >>= genIR
    -- >>= assignBlocks
    -- >>= grapher
    >>= codegen
  in formatAsm <$> asm

ir :: String -> Either String String
ir input = let
  out = parseProg input
    >>= weeder
    >>= resolver
    >>= typer
    >>= addresser
    >>= genIR
  in return $ show out
