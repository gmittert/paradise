module Main where

import System.Environment
import qualified System.Process as P
import Compile

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fname] -> do
      text <- readFile fname
      case process text of
        Right succ -> putStrLn succ
        Left err -> print err
      return ()
    _ -> putStrLn "Usage: jcc <input file>"
