module Main where

import System.Console.ArgParser
import qualified Data.Map as M
import Lib.Types
import qualified Ast.ParsedAst as PA

import Compile
import Importer

data CmdArgs = CmdArgs {
  printIR :: Bool
  , printAsm :: Bool
  , native :: Bool
  , filename :: String
}

argsParser :: ParserSpec CmdArgs
argsParser = CmdArgs
  `parsedBy` boolFlag "ir" `Descr` "Print out the IR instead of producing a binary"
  `andBy` boolFlag "asm" `Descr` "Print out the ASM instead of producing a binary"
  `andBy` boolFlag "native" `Descr` "Create x86_64 instead of C"
  `andBy` reqPos "filename" `Descr` "The file to compile"

argsInterface :: IO (CmdLnInterface CmdArgs)
argsInterface = (`setAppDescr` "Compiles .al files to x86")
  <$> mkApp argsParser

main :: IO ()
main = do
  interface <- argsInterface
  runApp interface compileTarget

cmd :: CmdArgs -> M.Map ModulePath PA.Module -> Either String String
cmd args
  | printIR args = ir
  | printAsm args = asm
  | native args = compile
  | otherwise = compileC

pathToName :: String -> String
pathToName p = (\x -> if x == '/' then '_' else x) <$> p

postCmd :: CmdArgs -> String -> IO ()
postCmd args
  | printIR args = putStrLn
  | printAsm args = putStrLn
  | native args = \succ -> do
      let output = "/tmp/" ++  pathToName (filename args) ++ ".S"
      writeFile output succ
      makeExecutable "a.out" [output]
  | otherwise = \succ -> do
      let output = "/tmp/" ++  pathToName (filename args) ++ ".c"
      writeFile output succ
      cToExe "a.out" output

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
