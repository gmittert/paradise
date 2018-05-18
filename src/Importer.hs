{- |
Module      : Importer
Description : The Importer handles finding imported modules and parsing them
Copyright   : (c) Jason Mittertreiner, 2018
-}
module Importer where

import qualified Data.Map.Strict as M
import Lib.Types
import qualified Ast.ParsedAst as PA
import Parser
import Errors.CompileError
import Errors.ImporterError
import Errors.ParseError

importer :: String -> String -> IO (Either CompileError (M.Map ModulePath PA.Module))
importer fname text = case parseModule text of
                  Left s -> return $ Left $ ImporterE $ ImporterError s
                  Right (PA.Module _ imports cfuncs funcs p) ->
                    let renamed = PA.Module fname (fileToModulePath "stdlib/io.para" : imports) cfuncs funcs p in
                    resolveImports (getImports renamed) (M.singleton (fileToModulePath fname) renamed)

getImports :: PA.Module -> [ModulePath]
getImports (PA.Module _ imprts _ _ _) = imprts

resolveImports :: [ModulePath] -> M.Map ModulePath PA.Module -> IO (Either CompileError (M.Map ModulePath PA.Module))
resolveImports [] m = return $ Right m
resolveImports (x:xs) m = case M.lookup x m of
  Just _ -> resolveImports xs m
  Nothing -> do
    modl <- getImport x
    case modl of
      Left s -> return $ Left s
      Right (PA.Module _ imports cfuncs funcs p) -> do
        let modl' = PA.Module (modulePathToFile x) imports cfuncs funcs p
        let newImports = getImports modl'
        resolveImports (newImports ++ xs) (M.insert x modl' m)

getImport :: ModulePath -> IO (Either CompileError PA.Module)
getImport p = do
  let fname = modulePathToFile p
  contents <- readFile $ fname
  case parseModule contents of
    Right a -> return $ Right a
    Left a -> return $ Left $ ParseE (ParseError fname a)
