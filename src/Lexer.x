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
  int                   { \s -> TokenInt }
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

{
data Token =
  TokenNum Int
  | TokenInt
  | TokenTrue
  | TokenFalse
  | TokenMain
  | TokenReturn
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
  deriving (Eq, Show)

scanTokens = alexScanTokens
}
