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
  main                  { \s -> TokenMain }
  char                  { \s -> TokenCharDec }
  string                { \s -> TokenStringDec }
  bool                  { \s -> TokenBoolDec }
  return                { \s -> TokenReturn }
  true                  { \s -> TokenTrue }
  false                 { \s -> TokenFalse }
  while                 { \s -> TokenWhile }
  for                   { \s -> TokenFor }
  [A-Za-z][A-Za-z0-9]*  { \s -> TokenSym s }
  \;                    { \s -> TokenSemi }
  \,                    { \s -> TokenComma }
  \{                    { \s -> TokenLbrace }
  \}                    { \s -> TokenRbrace }
  \(                    { \s -> TokenLparen }
  \)                    { \s -> TokenRparen }
  \=                    { \s -> TokenAssign }
  \+                    { \s -> TokenPlus }
  \-                    { \s -> TokenMinus }
  \*                    { \s -> TokenStar }
  \/                    { \s -> TokenStar }
  \[                    { \s -> TokenLbrack}
  \]                    { \s -> TokenRbrack}
  \'[^']\'              { \s -> TokenChar (read s :: Char)}
  \"[^\"]*\"            { \s -> TokenString (read s :: String)}

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
  | TokenLbrack
  | TokenRbrack
  | TokenAssign
  | TokenPlus
  | TokenMinus
  | TokenStar
  | TokenPrint
  | TokenString String
  | TokenChar Char
  | TokenCharDec
  | TokenStringDec
  | TokenBoolDec
  | TokenIntDec
  | TokenComma
  | TokenWhile
  | TokenFor
  | TokenDiv
  deriving (Eq, Show)

scanTokens = alexScanTokens
}
