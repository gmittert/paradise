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
import System.Process

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
  compileC (M.singleton (ModulePath ["test"]) (PA.Module "test.al" imports funcs))

compileC :: M.Map ModulePath PA.Module -> Either String String
compileC input = let
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

cToExe :: FilePath -> FilePath -> IO()
cToExe target input = callCommand $ "clang " ++ input ++ " -o " ++ target ++ " "
