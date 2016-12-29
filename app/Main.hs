module Main where

import Parser (parseProg)
import TAC
import System.Environment
import qualified System.Process as P
import CodeGen
import Optimize
import Asm

process :: String -> Either String [AInstr]
process input = do
  let parsed = parseProg input
  case parsed of
    Right ast -> Right $ (codeGen . optimize . compile) ast
    Left err -> Left $ "Parser Error:" ++ err

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fname] -> do
      lines <- readFile fname
      case process lines of
        Right asm -> writeFile (fname ++ ".s") (formatAsm asm)
        Left err -> print err
      _ <- P.createProcess (P.proc "/usr/bin/gcc" [fname ++ ".s", "-o" ++ fname ++ ".elf"])
      return ()
    _ -> putStrLn "Usage: jlc <input file>"

formatAsm :: [AInstr] -> String
formatAsm = foldr (\x y -> show x ++ y) ""
