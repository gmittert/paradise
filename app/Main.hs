module Main where

import Parser (parseProg)
import System.Environment
import qualified System.Process as P
import Asm
import Resolver
import Typer
import Grapher
import Lib.IR
import Lib.Graph
import GenIR
import Codegen
import BasicBlocks

process :: String -> Either String [AInstr]
process input = parseProg input
  >>= resolver
  >>= typer
  >>= genIR
  >>= assignBlocks
  >>= grapher
  >>= codegen

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fname] -> do
      text <- readFile fname
      case process text of
        Right succ -> do
          writeFile (fname ++ ".out") $ formatAsm succ
        Left err -> print err
      return ()
    _ -> putStrLn "Usage: jcc <input file>"

formatAsm :: [AInstr] -> String
formatAsm = foldr (\x y -> show x ++ y) ""
