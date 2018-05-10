module Main where

import System.Console.ArgParser
import qualified Data.Map as M
import Lib.Types
import qualified Ast.ParsedAst as PA
import Args
import qualified Data.ByteString as BS
import Errors.CompileError

import Compile
import Importer

main :: IO ()
main = do
  interface <- argsInterface
  runApp interface compileTarget

cmd :: CmdArgs -> M.Map ModulePath PA.Module -> IO (Either CompileError BS.ByteString)
cmd args
  | printLLVM args = compile
  | otherwise = compile

pathToName :: String -> String
pathToName p = (\x -> if x == '/' then '_' else x) <$> p

postCmd :: CmdArgs -> BS.ByteString -> IO ()
postCmd args
  | printLLVM args = BS.putStr
  | otherwise = \s -> do
      let output = "/tmp/" ++  pathToName (filename args) ++ ".ll"
      BS.writeFile output s
      llvmToExe output args

compileTarget :: CmdArgs -> IO ()
compileTarget args = do
  text <- readFile (filename args)
  imported <- importer (filename args) text
  case imported of
    Right imported' -> do
      res <- cmd args imported' 
      case res of
        Right succ -> postCmd args succ
        Left err -> print err
    Left f -> print f
  return ()
