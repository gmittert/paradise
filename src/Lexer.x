{
module Lexer (
 Token(..),
 scanTokens
) where

import Syntax
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-

  $white+               ;
  "//".*                ;
  $digit+               { \s -> TokenNum (read s) }
  int                   { \s -> TokenIntDec }
  char                  { \s -> TokenCharDec }
  string                { \s -> TokenStringDec }
  bool                  { \s -> TokenBoolDec }
  main                  { \s -> TokenMain }
  return                { \s -> TokenReturn }
  true                  { \s -> TokenTrue }
  print                 { \s -> TokenPrint }
  false                 { \s -> TokenFalse }
  [A-Za-z][A-Za-z0-9]*  { \s -> TokenSym s }
  \;                    { \s -> TokenSemi }
  \{                    { \s -> TokenLbrace }
  \}                    { \s -> TokenRbrace }
  \(                    { \s -> TokenLparen }
  \)                    { \s -> TokenRparen }
  \=                    { \s -> TokenAssign }
  \+                    { \s -> TokenPlus }
  \-                    { \s -> TokenMinus }
  \'.\'                 { \s -> TokenChar (read s :: Char)}
  \"[^\"]\"             { \s -> TokenString (read s :: String)}

{
data Token =
  TokenNum Int
  | TokenInt
  | TokenTrue
  | TokenFalse
  | TokenMain
  | TokenReturn
  | TokenBool Bool
  | TokenSym String
  | TokenSemi
  | TokenLbrace
  | TokenRbrace
  | TokenLparen
  | TokenRparen
  | TokenAssign
  | TokenPlus
  | TokenMinus
  | TokenMult
  | TokenDiv
  | TokenPrint
  | TokenString String
  | TokenChar Char
  | TokenCharDec
  | TokenStringDec
  | TokenBoolDec
  | TokenIntDec
  deriving (Eq, Show)

scanTokens = alexScanTokens
}
