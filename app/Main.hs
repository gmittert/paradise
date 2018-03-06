module Main where

import System.Console.ArgParser

import Compile
import Importer

data CmdArgs = CmdArgs {
  printIR :: Bool
  , printAsm :: Bool
  , filename :: String
}

argsParser :: ParserSpec CmdArgs
argsParser = CmdArgs
  `parsedBy` boolFlag "ir" `Descr` "Print out the IR instead of producing a binary"
  `andBy` boolFlag "asm" `Descr` "Print out the ASM instead of producing a binary"
  `andBy` reqPos "filename" `Descr` "The file to compile"

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
  case imported of
    Right imported' -> case (if printIR args then ir else (if printAsm args then asm else compile)) imported' of
        Right succ -> 
          if not (printIR args) && not  (printAsm args) then do
            let output = "/tmp/" ++ ((\x -> if x == '/' then '_' else x) <$> filename args) ++ ".S"
            writeFile output succ
            makeExecutable "a.out" [output]
            else putStrLn succ
        Left err -> print err
    Left f -> putStrLn f
  return ()
