module Compile where

import Parser (parseProg)
import Lib.Asm
import Resolver
import Weeder
import Typer
import Grapher
import Lib.IR
import Addresser
import GenIR
import Codegen
import BasicBlocks
import Canonicalizer
import Ast.TypedAst
import Ast.ResolvedAst
import Control.Monad.State.Lazy

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
--    >>= codegen
  in case asm of
    Right res -> Right $ show $ evalState (irgen res) Lib.IR.emptyState
    Left err -> Left err
