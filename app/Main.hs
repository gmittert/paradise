module Main where

import Parser (parseProg)
import TAC
import System.Environment
import qualified System.Process as P
import CodeGen
import Optimize
import Asm
import Syntax
import Semantic

process :: String -> Either String [AInstr]
process input = case parseProg input of
    Right ast -> Right $ (codeGen . optimize . compile) ast
    Left err -> Left $ "Parser Error:" ++ err

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--parse", fname] -> do
      text <- readFile fname
      case parseProg text of
        Right asm -> print asm
        Left err -> print err
    ["--semantic", fname] -> do
      text <- readFile fname
      case parseProg text of
        Right asm -> print (semanticAnalysis asm)
        Left err -> print err
    [fname] -> do
      text <- readFile fname
      case process text of
        Right asm -> writeFile (fname ++ ".s") (formatAsm asm)
        Left err -> print err
      _ <- P.createProcess (P.proc "/usr/bin/gcc" [fname ++ ".s", "-o" ++ fname ++ ".elf"])
      return ()
    _ -> putStrLn "Usage: jlc <input file>"

formatAsm :: [AInstr] -> String
formatAsm = foldr (\x y -> show x ++ y) ""
