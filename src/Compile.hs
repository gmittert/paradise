module Compile where

import Parser (parseProg)
import Asm
import Resolver
import Weeder
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
    >>= weeder
    >>= resolver
    >>= typer
    -- >>= offsets
    >>= genIR
    -- >>= assignBlocks
    -- >>= grapher
    >>= codegen
  in formatAsm <$> asm
