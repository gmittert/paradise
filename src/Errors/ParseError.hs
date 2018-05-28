module Errors.ParseError where
import Parser
import Lexer

data ParseError = ParseError {
  file :: String
  , tok :: [Token]
  }
  deriving (Eq)

instance Show ParseError where
  show (ParseError file []) = "Parse error while parsing " ++ file ++ ":\n\nUnexpected end of input"
  show (ParseError file (t:_)) = "Parse error while parsing " ++ file ++ ":\n\nOn token: " ++ fmtTok t
