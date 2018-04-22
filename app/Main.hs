module Main where

import System.Console.ArgParser
import qualified Data.Map as M
import Lib.Types
import qualified Ast.ParsedAst as PA
import System.Process
import Args

import Compile
import Importer

main :: IO ()
main = do
  interface <- argsInterface
  runApp interface compileTarget

cmd :: CmdArgs -> M.Map ModulePath PA.Module -> Either String String
cmd args
  | printC args = compile
  | otherwise = compile

pathToName :: String -> String
pathToName p = (\x -> if x == '/' then '_' else x) <$> p

postCmd :: CmdArgs -> String -> IO ()
postCmd args
  | printC args = \s -> do
      let output = "/tmp/" ++  pathToName (filename args) ++ ".c"
      writeFile output s
      callCommand $ "clang-format " ++ output
      putStrLn ""
  | otherwise = \succ -> do
      let output = "/tmp/" ++  pathToName (filename args) ++ ".c"
      writeFile output succ
      cToExe output args

compileTarget :: CmdArgs -> IO ()
compileTarget args = do
  text <- readFile (filename args)
  imported <- importer (filename args) text
  case imported of
    Right imported' -> case cmd args imported' of
        Right succ -> postCmd args succ
        Left err -> putStrLn err
    Left f -> putStrLn f
  return ()
