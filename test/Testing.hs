{-# LANGUAGE LambdaCase #-}
module Testing where

import Args
import Driver
import Control.Monad
import qualified Data.ByteString as BS
import Errors.CompileError
import Lib.Types
import System.Exit
import System.Process

run ::
     Either CompileError [(ModulePath, BS.ByteString)]
  -> IO (Either CompileError (Int, String, String))
run instrs =
  case instrs of
    (Left s) -> return (Left s)
    (Right mods) -> do
      let modToPath p = "/tmp/" ++ show p ++ ".ll"
      let args = CmdArgs False True False "/tmp/a.out" ""
      forM_ mods $ \(mod, code) -> do
        let output = modToPath mod
        BS.writeFile output code
      llvmToExe (map (modToPath . fst) mods) args
      (exit, stdout, stderr) <- readProcessWithExitCode "/tmp/a.out" [] ""
      return $
        Right
          ( case exit of
              ExitSuccess -> 0
              ExitFailure e -> e
          , stdout
          , stderr)

exitOf :: String -> IO (Either CompileError Int)
exitOf fname = do
  instrs <- compileFile fname
  run instrs >>= \case
    Right (a, _, _) -> return $ Right a
    Left s -> return $ Left s

stdoutOf :: String -> IO (Either CompileError String)
stdoutOf fname = do
  instrs <- compileFile fname
  run instrs >>= \case
    Right (_, a, _) -> return $ Right a
    Left s -> return $ Left s

stderrOf :: String -> IO (Either CompileError String)
stderrOf fname = do
  instrs <- compileFile fname
  run instrs >>= \case
    Right (_, _, a) -> return $ Right a
    Left s -> return $ Left s

isTypeError :: String -> IO Bool
isTypeError fname = do
  exit <- exitOf fname 
  return $ case exit of
    (Left (TyperE _)) -> True
    _ -> False
