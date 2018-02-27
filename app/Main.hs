module Main where

import System.Console.ArgParser
import Compile
import Importer

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
  runApp interface compileTarget

compileTarget :: CmdArgs -> IO ()
compileTarget args = do
  text <- readFile (filename args)
  imported <- importer (filename args) text
  case imported of
    Right imported' -> case (if printIR args then ir else compile) imported' of
        Right succ -> putStrLn succ
        Left err -> print err
    Left f -> putStrLn f
  return ()
