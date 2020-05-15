module Driver where

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
import Errors.ParseError

compileFile :: String -> IO (Either CompileError [(ModulePath, BS.ByteString)])
compileFile name = do
  text <- readFile name
  imported <- importer name text
  case imported of
    Right imported' -> compile imported'
    Left s -> return $ Left s

compileString :: String -> IO (Either CompileError [(ModulePath, BS.ByteString)])
compileString s = do
  let mods = parseModule ("module test\n" ++ s)
  case mods of
    Right (PA.Module _ imports cfuncs funcs p) -> compile (M.singleton (ModulePath ["test"]) (PA.Module "test.para" imports cfuncs funcs p))
    Left t -> return $ Left $ ParseE $ ParseError s t

compile :: M.Map ModulePath PA.Module -> IO (Either CompileError [(ModulePath, BS.ByteString)])
compile input = let
  c = weeder input
    >>= resolver
    >>= typer
    >>= genLLVM
  in case c of
    Right asts ->
      -- Use LLVM-hs-pretty if we just want the llvm code
      return $ return $ M.toList (M.map (read . show . ppllvm) (fst <$> asts))
    Left err -> return $ Left err

-- | Turn a filepath into a file name by replacing slashes with '_'s
flattenPath :: String -> String
flattenPath = map (\x -> if x == '/' then '_' else x)


-- | Compile a .ll file
llvmToExe :: [FilePath] -> CmdArgs -> IO()
llvmToExe input args = let
  flags = [
    if printAsm args then "" else "-o " ++ o args
    -- , if printAsm args then "" else "-l OpenCL"
    , if debug args then "-g" else ""
    , if printAsm args then "-S" else ""
    ] in
  callCommand $ "clang " ++ unwords input ++ " " ++ unwords flags
