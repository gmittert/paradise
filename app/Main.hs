module Main where

import Parser (parseProg)
import TAC
import System.Environment

process :: String -> IO ()
process input = do
  let parsed = parseProg input
  case parsed of
    Right ast -> print $ compile ast
    Left err -> do
      putStrLn "Parser Error:"
      print err

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "Usage: jlc <input file>"
    [fname] -> do
      contents <- readFile fname
      process contents
