module Args where

import System.Console.ArgParser

-- | The command line arguments for the program
data CmdArgs = CmdArgs {
  debug :: Bool
  , printLLVM :: Bool
  , o :: String
  , filename :: String
}

argsParser :: ParserSpec CmdArgs
argsParser = CmdArgs
             `parsedBy` boolFlag "debug" `Descr` "Include debug symbols"
             `andBy` boolFlag "printLLVM" `Descr` "Print the llvm instead of compiling"
             `andBy` optFlag "a.out" "o" `Descr` "The output executable name"
             `andBy` reqPos "filename" `Descr` "The file to compile"

argsInterface :: IO (CmdLnInterface CmdArgs)
argsInterface = (`setAppDescr` "Compiles .para files")
  <$> mkApp argsParser

