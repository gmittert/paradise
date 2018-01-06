{- |
Module      : Preprocessor
Description : The preprocessor does text insertion and replacement based on
               preprocessor directives
Copyright   : (c) Jason Mittertreiner, 2018
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Preprocessor where

import Control.Monad
import Data.Maybe
import Control.Monad.State.Class
import Control.Monad.State.Lazy
import System.Directory
import qualified Data.Map.Strict as M

preprocessor :: String -> IO String
preprocessor file = join $ evalStateT (runPreprocessor (preprocessFile file)) emptyState

preprocessFile :: String -> Preprocessor (IO String)
preprocessFile file = do
  lines <- forM (lines file) preprocessLine
  return $ concat <$> sequence lines

data PreprocessorState
  = PreprocessorState {
    defs :: M.Map String String
    , ifdepth :: Int
    , ignoring :: Bool
    , ignoreUntil :: Int
  }
  deriving (Eq, Ord, Show)

emptyState :: PreprocessorState
emptyState = PreprocessorState M.empty 0 False 0

newtype Preprocessor a = Preprocessor { runPreprocessor :: StateT PreprocessorState IO a }
  deriving (Functor, Applicative, Monad, MonadState PreprocessorState)

dropWS :: String -> String
dropWS = dropWhile (\x -> x == ' ' || x == '\t')

strip :: String -> String
strip = reverse . dropWS . reverse . dropWS

isDirective :: String -> Bool
isDirective = (== '#') . head

preprocessLine :: String -> Preprocessor (IO String)
preprocessLine l = let stripped = strip l in
  if isDirective stripped then doDirective stripped else preprocess stripped

-- | Replaces a word if it is in the list of defines dict
replaceWord :: String -> Preprocessor (IO String)
replaceWord s = do
  dict <- gets defs
  return $ return $ fromMaybe s (M.lookup s dict)

{- | Preprocess a non directive line
   - Replace all defines
-}
preprocess :: String -> Preprocessor (IO String)
preprocess s = do
  let split = words s
  replaced <- forM split replaceWord
  return $ concat <$> sequence replaced

doDirective :: String -> Preprocessor (IO String)
doDirective d = let command = words (tail d)
                    directive = head command
                    args = tail command
                    in
  case directive of
    "include" -> doInclude args
    "define" -> doDefine args
    "ifdef" -> doIfdef args
    "ifndef" -> doIfndef args
    "else" -> doElse
    "endif" -> doEndif
    _ -> error $ "Undefined preprocessor directive: " ++ directive

includePath :: [String]
includePath = ["/usr/include", "/usr/local/include"]

findFile :: [String] -> String -> IO String
findFile locs fname = let
  files = map (++ "/" ++ fname) locs
  in head <$> filterM doesFileExist files

getStrContents :: String -> String
getStrContents = takeWhile (/= '"') . dropWhile (/= '"')

doInclude :: [String] -> Preprocessor (IO String)
doInclude arg = do
  let name = getStrContents (concat arg)
  return $ do contents <- readFile name
              preprocessor contents

-- | Insert a macro definition into the dictionary
doDefine :: [String] -> Preprocessor (IO String)
doDefine args = do
  defs' <- defs <$> get
  case args of
    -- #define x y
    x:xs -> modify $ \s -> s{ defs = M.insert x (unwords xs) defs' }
    [] -> error "#define given without arguments"
  return $ return ""

doIfdef :: [String] -> Preprocessor (IO String)
doIfdef args = case args of
  [x] -> do
    s <- get
    modify $ \st -> st{ifdepth = ifdepth st + 1}
    let val = M.lookup x (defs s)
    let ignore = ignoring s
    when (not ignore && isNothing val) $
          modify $ \st -> st{
            ignoring = True
            , ignoreUntil = ifdepth st
            }
    return $ return ""
  _ -> error "ifdef should take one arg"

doIfndef :: [String] -> Preprocessor (IO String)
doIfndef args = case args of
  [x] -> do
    s <- get
    modify $ \st -> s{ifdepth = ifdepth st + 1}
    let val = M.lookup x (defs s)
    let ignore = ignoring s
    when (not ignore && isJust val) $
          modify $ \st -> st{
            ignoring = True
            , ignoreUntil = ifdepth st
            }
    return $ return ""
  _ -> error "ifndef should take one arg"

doElse :: Preprocessor (IO String)
doElse = do
  s <- get
  when (ifdepth s == ignoreUntil s) $ 
    modify $ \s -> s{ ignoring = (not . ignoring) s}
  return $ return ""

doEndif :: Preprocessor (IO String)
doEndif = do
  s <- get
  modify $ \s -> s{ ifdepth = ifdepth s - 1}
  when (ifdepth s == ignoreUntil s) $
    modify $ \s -> s{ ignoring = False}
  return $ return ""
