module Main where

import Parser (parseProg)
import System.Environment
import qualified System.Process as P
import Asm
import Resolver
import Ast.ResolvedAst

process :: String -> Either String ResolvedAst
process input = parseProg input
  >>= resolver

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fname] -> do
      text <- readFile fname
      case process text of
        Right succ -> do
          writeFile (fname ++ ".out") "Success"
        Left err -> print err
      return ()
    _ -> putStrLn "Usage: jcc <input file>"

formatAsm :: [AInstr] -> String
formatAsm = foldr (\x y -> show x ++ y) ""
