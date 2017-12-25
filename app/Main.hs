module Main where

import System.Console.ArgParser
import Compile

data CmdArgs = CmdArgs {
  printIR :: Bool
  , filename :: String
}

argsParser :: ParserSpec CmdArgs
argsParser = CmdArgs
  `parsedBy` boolFlag "ir" `Descr` "Print out the IR instead of assembly"
  `andBy` reqPos "filename" `Descr` "The file to compile"

argsInterface :: IO (CmdLnInterface CmdArgs)
argsInterface = (`setAppDescr` "Compiles .c files to x86")
  <$> mkApp argsParser

main :: IO ()
main = do
  interface <- argsInterface
  runApp interface compileFile

compileFile :: CmdArgs -> IO ()
compileFile args = do
  text <- readFile (filename args)
  case (if printIR args then ir else compile) text of
    Right succ -> putStrLn succ
    Left err -> print err
  return ()
