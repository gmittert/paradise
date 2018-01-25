module Compile where

import Parser
import Importer
import Resolver
import Weeder
import Typer
import Lib.IR
import Addresser
import GenIR
import BasicBlocks
import Canonicalizer
import ConstantFolder
import Codegen
import Control.Monad.State.Lazy
import Ast.ParsedAst as PA
import qualified Data.Map.Strict as M
import Lib.Types

import Lib.Asm

compileFile :: String -> IO (Either String String)
compileFile name = do
  text <- readFile name
  imported <- importer text
  return $ case imported of
    Right imported' -> compile imported'
    Left s -> Left s

compileString :: String -> Either String String
compileString s = do
  mod <- parseModule ("module test\n" ++ s)
  compile (M.singleton (ModulePath ["test"]) mod)

compile :: M.Map ModulePath PA.Module -> Either String String
compile input = let
  asm = weeder input
    >>= resolver
    >>= typer
    >>= addresser
    >>= genIR
    >>= canonicalize
    >>= constFold
    >>= basicBlocks
    >>= codegen
  in case asm of
    Right res -> Right $ formatAsm $ evalState (irgen res) Lib.IR.emptyState
    Left err -> Left err

ir :: M.Map ModulePath PA.Module -> Either String String
ir input = let
  asm = weeder input
    >>= resolver
    >>= typer
    >>= addresser
    >>= genIR
    >>= canonicalize
    >>= constFold
    >>= basicBlocks
  in case asm of
    Right res -> Right $ show $ evalState (irgen res) Lib.IR.emptyState
    Left err -> Left err
