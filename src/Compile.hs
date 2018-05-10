module Compile where

import Parser
import Importer
import Resolver
import Weeder
import Typer
import GenLLVM
import Ast.ParsedAst as PA
import qualified Data.Map.Strict as M
import Lib.Types
import Args
import System.Process
import LLVM.Module
import LLVM.Context
import Lib.Llvm
import qualified Data.ByteString as BS
import Errors.CompileError
import Errors.ImporterError

compileFile :: String -> IO (Either CompileError BS.ByteString)
compileFile name = do
  text <- readFile name
  imported <- importer name text
  case imported of
    Right imported' -> compile imported'
    Left s -> return $ Left s

compileString :: String -> IO (Either CompileError BS.ByteString)
compileString s = do
  let mods = parseModule ("module test\n" ++ s)
  case mods of
    Right (PA.Module _ imports funcs) -> compile (M.singleton (ModulePath ["test"]) (PA.Module "test.para" imports funcs))
    Left e -> return $ Left $ ImporterE $ ImporterError e

compile :: M.Map ModulePath PA.Module -> IO (Either CompileError BS.ByteString)
compile input = let
  c = weeder input
    >>= resolver
    >>= typer
    >>= genLLVM (emptyModule "empty mod")
  in case c of
    Right ast -> LLVM.Context.withContext $ \context -> withModuleFromAST context ast $ \m -> do
      llstr <- moduleLLVMAssembly m
      return (Right llstr)
    Left err -> return $ Left err

flattenPath :: String -> String
flattenPath = map (\x -> if x == '/' then '_' else x)

link :: FilePath -> [FilePath] -> IO ()
link target files = do
   let flist = concatMap ((:) ' ') files
   callCommand $ "ld " ++ flist ++ " -o " ++ target

llvmToExe :: FilePath -> CmdArgs -> IO()
llvmToExe input args = let
  flags = [
    "-o " ++ o args
    , "-l OpenCL"
    , if debug args then "-g" else ""
    ] in
  callCommand $ "clang " ++ input ++ " " ++ unwords flags
