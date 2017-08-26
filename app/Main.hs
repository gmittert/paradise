module Main where

import System.Environment
import qualified System.Process as P
import Compile
import Data.List


main :: IO ()
main = do
  args <- getArgs
  case filter (\x -> head x /= '-') args of
    [fname] -> do
      text <- readFile fname
      let doIR = "--ir" `elem` args
      case (if doIR then ir else process) text of
        Right succ -> putStrLn succ
        Left err -> print err
      return ()
    _ -> putStrLn "Usage: jcc <input file>"
