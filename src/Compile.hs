module Compile where

import Parser
import Importer
import Resolver
import Weeder
import Typer
import GenC
import Ast.ParsedAst as PA
import qualified Data.Map.Strict as M
import Lib.Types
import Args
import System.Process

compileFile :: String -> IO (Either String String)
compileFile name = do
  text <- readFile name
  imported <- importer name text
  return $ case imported of
    Right imported' -> compile imported'
    Left s -> Left s

compileString :: String -> Either String String
compileString s = do
  (PA.Module _ imports funcs) <- parseModule ("module test\n" ++ s)
  compile (M.singleton (ModulePath ["test"]) (PA.Module "test.para" imports funcs))

compile :: M.Map ModulePath PA.Module -> Either String String
compile input = let
  c = weeder input
    >>= resolver
    >>= typer
    >>= runCGen
  in case c of
    Right c -> Right $ show c
    Left err -> Left err

flattenPath :: String -> String
flattenPath = map (\x -> if x == '/' then '_' else x)

link :: FilePath -> [FilePath] -> IO ()
link target files = do
   let flist = concatMap ((:) ' ') files
   callCommand $ "ld " ++ flist ++ " -o " ++ target

cToExe :: FilePath -> CmdArgs -> IO()
cToExe input args = let
  flags = [
    "-o " ++ o args
    , "-l OpenCL"
    , if debug args then "-g" else ""
    ] in
  callCommand $ "clang " ++ input ++ " " ++ unwords flags
