module Main where

import System.Environment
import Compile

main :: IO ()
main = do
  args <- getArgs
  case filter (\x -> head x /= '-') args of
    [fname] -> do
      text <- readFile fname
      case compile text of
        Right succ -> putStrLn succ
        Left err -> print err
      return ()
    _ -> putStrLn "Usage: jcc <input file>"
