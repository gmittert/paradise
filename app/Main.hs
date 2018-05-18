{-# LANGUAGE LambdaCase #-}

module Main where

import Args
import Control.Monad
import qualified Data.ByteString as BS
import Lib.Types
import System.Console.ArgParser

import Compile
import Importer

main :: IO ()
main = do
  interface <- argsInterface
  runApp interface compileTarget

pathToName :: String -> String
pathToName =
  map $
    \case
       '/' -> '_'
       a -> a

postCmd :: CmdArgs -> [(ModulePath, BS.ByteString)] -> IO ()
postCmd args mods
  | printLLVM args =
    forM_ mods $ \(mod, code) -> do
      putStr ("Module: " ++ show mod ++ "\n")
      BS.putStr code
  | otherwise = do
    let modToPath p = "/tmp/" ++ show p ++ ".ll"
    forM_ mods $ \(mod, code) -> do
      let output = modToPath mod
      BS.writeFile output code
    llvmToExe (map (modToPath . fst) mods) args

compileTarget :: CmdArgs -> IO ()
compileTarget args = do
  text <- readFile (filename args)
  imported <- importer (filename args) text
  case imported of
    Right imported' -> do
      res <- compile args imported'
      case res of
        Right succ -> postCmd args succ
        Left err -> print err
    Left f -> print f
  return ()
