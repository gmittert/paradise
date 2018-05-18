module Errors.ParseError where

data ParseError = ParseError {
  file :: String
  , message :: String
  }
  deriving (Eq, Ord)

instance Show ParseError where
  show (ParseError file message) = "Parse Error while parsing " ++ file ++ ":\n\n" ++ message
