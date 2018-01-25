{- |
Module      : Importer
Description : The Importer handles 
Copyright   : (c) Jason Mittertreiner, 2018
-}
module Importer where

import qualified Data.Map.Strict as M
import Lib.Types
import qualified Ast.ParsedAst as PA
import Parser

importer :: String -> IO (Either String (M.Map ModulePath PA.Module))
importer file = case parseModule file of
                  Left s -> return $ Left s
                  Right parsed@(PA.Module name _ _) ->
                    resolveImports (getImports parsed) (M.singleton (ModulePath [name]) parsed)

getImports :: PA.Module -> [ModulePath]
getImports (PA.Module _ imprts _) = imprts

resolveImports :: [ModulePath] -> M.Map ModulePath PA.Module -> IO (Either String (M.Map ModulePath PA.Module))
resolveImports [] m = return $ Right m
resolveImports (x:xs) m = case M.lookup x m of
  Just _ -> resolveImports xs m
  Nothing -> do
    modl <- getImport x
    case modl of
      Left s -> return $ Left s
      Right modl' -> do
        let newImports = getImports modl'
        resolveImports (newImports ++ xs) (M.insert x modl' m)

getImport :: ModulePath -> IO (Either String PA.Module)
getImport p = do
  contents <- readFile $ modulePathToFile p
  return $ parseModule contents
