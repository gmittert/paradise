module Compile where

import Parser (parseProg)
import Asm
import Resolver
import Typer
import Grapher
import Lib.IR
import Lib.Graph
import GenIR
import Codegen
import BasicBlocks
import Ast.TypedAst
import Ast.ResolvedAst

process :: String -> Either String String
process input = let
  asm = parseProg input
    >>= resolver
    >>= typer
    >>= genIR
    -- >>= assignBlocks
    -- >>= grapher
    >>= codegen
  in formatAsm <$> asm
