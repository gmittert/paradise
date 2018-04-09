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
import Reachability
import GenC
import Control.Monad.State.Lazy
import Ast.ParsedAst as PA
import qualified Data.Map.Strict as M
import Lib.Types
import System.Process

import Lib.Asm

compileFile :: String -> IO (Either String String)
compileFile name = do
  text <- readFile name
  imported <- importer name text
  return $ case imported of
    Right imported' -> compileC imported'
    Left s -> Left s

compileString :: String -> Either String String
compileString s = do
  (PA.Module _ imports funcs) <- parseModule ("module test\n" ++ s)
  compile (M.singleton (ModulePath ["test"]) (PA.Module "test.al" imports funcs))

compileC :: M.Map ModulePath PA.Module -> Either String String
compileC input = let
  c = weeder input
    >>= resolver
    >>= typer
    >>= genCProg
  in case c of
    Right c -> Right $ show c
    Left err -> Left err


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
    >>= reachability
    >>= codegen
  in case asm of
    Right res -> Right $ formatAsm $ evalState (irgen res) Lib.IR.emptyState
    Left err -> Left err

makeExecutable :: FilePath -> [FilePath] -> IO ()
makeExecutable output files = do
  let addedFiles = "stdlib/runtime.S":files
  forM addedFiles assemble >>= link output

flattenPath :: String -> String
flattenPath = map (\x -> if x == '/' then '_' else x)

assemble :: FilePath -> IO FilePath
assemble f = do
  let tmploc = "/tmp/" ++ flattenPath f ++ ".o"
  callCommand $ "as " ++ f ++ " -o " ++ tmploc
  return tmploc

link :: FilePath -> [FilePath] -> IO ()
link target files = do
   let flist = concatMap ((:) ' ') files
   callCommand $ "ld " ++ flist ++ " -o " ++ target

cToExe :: FilePath -> FilePath -> IO()
cToExe target input = callCommand $ "clang " ++ input ++ " -o " ++ target ++ " "

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
    >>= reachability
  in case asm of
    Right res -> Right $ show $ evalState (irgen res) Lib.IR.emptyState
    Left err -> Left err

asm :: M.Map ModulePath PA.Module -> Either String String
asm input = let
  asm = weeder input
    >>= resolver
    >>= typer
    >>= addresser
    >>= genIR
    >>= canonicalize
    >>= constFold
    >>= basicBlocks
    >>= reachability
    >>= codegen
  in case asm of
    Right res -> Right $ show $ evalState (irgen res) Lib.IR.emptyState
    Left err -> Left err
