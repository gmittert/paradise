module Main where

import System.Console.ArgParser
import Control.Monad

import Compile
import Importer

data CmdArgs = CmdArgs {
  printIR :: Bool
  , filename :: String
  , asmFiles :: [String]
}

argsParser :: ParserSpec CmdArgs
argsParser = CmdArgs
  `parsedBy` boolFlag "ir" `Descr` "Print out the IR instead of assembly"
  `andBy` reqPos "filename" `Descr` "The file to compile"
  `andBy` optFlagArgs [] "asmfiles" [] (flip (:)) `Descr` "Additional asm files to include"

argsInterface :: IO (CmdLnInterface CmdArgs)
argsInterface = (`setAppDescr` "Compiles .al files to x86")
  <$> mkApp argsParser

main :: IO ()
main = do
  interface <- argsInterface
  runApp interface compileTarget

compileTarget :: CmdArgs -> IO ()
compileTarget args = do
  text <- readFile (filename args)
  imported <- importer (filename args) text
  let stdlib = ["stdlib/io.S"]
  extraAsm <- sequence $ join $ forM [asmFiles args ++ stdlib] (liftM readFile)
  case imported of
    Right imported' -> case (if printIR args then ir else compile) imported' of
        Right succ -> putStrLn (succ ++ join extraAsm)
        Left err -> print err
    Left f -> putStrLn f
  return ()
