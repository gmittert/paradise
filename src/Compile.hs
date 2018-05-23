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
import LLVM.Pretty (ppllvm)
import qualified Data.ByteString as BS
import Errors.CompileError
import Errors.ImporterError

compileFile :: String -> IO (Either CompileError [(ModulePath, BS.ByteString)])
compileFile name = do
  text <- readFile name
  imported <- importer name text
  case imported of
    Right imported' -> compile (defaultArgs name) imported'
    Left s -> return $ Left s

compileString :: String -> IO (Either CompileError [(ModulePath, BS.ByteString)])
compileString s = do
  let mods = parseModule ("module test\n" ++ s)
  case mods of
    Right (PA.Module _ imports cfuncs funcs p) -> compile (defaultArgs "test.para") (M.singleton (ModulePath ["test"]) (PA.Module "test.para" imports cfuncs funcs p))
    Left e -> return $ Left $ ImporterE $ ImporterError e

compile :: CmdArgs -> M.Map ModulePath PA.Module -> IO (Either CompileError [(ModulePath, BS.ByteString)])
compile args input = let
  c = weeder input
    >>= resolver
    >>= typer
    >>= genLLVM
  in case c of
    Right asts ->
      if printLLVM args
      then
        -- Use LLVM-hs-pretty if we just want the llvm code
        return $ return $ M.toList (M.map (read . show . ppllvm) asts)
      else do
        return $ return $ M.toList (M.map (read . show . ppllvm) asts)
        -- cmp <- mapM (\ast -> LLVM.Context.withContext $ \context -> withModuleFromAST context ast $ fmap Right . moduleLLVMAssembly) asts
        -- let seqed = sequence cmp
        -- return $ M.toList <$> seqed
    Left err -> return $ Left err

-- | Turn a filepath into a file name by replacing slashes with '_'s
flattenPath :: String -> String
flattenPath = map (\x -> if x == '/' then '_' else x)

-- | Compile a .ll file
llvmToExe :: [FilePath] -> CmdArgs -> IO()
llvmToExe input args = let
  flags = [
    "-o " ++ o args
    , "-l OpenCL"
    , if debug args then "-g" else ""
    ] in
  callCommand $ "clang " ++ unwords input ++ " " ++ unwords flags
