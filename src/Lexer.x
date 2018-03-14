{
module Lexer (
 Token(..),
 scanTokens
) where

import Ast.ParsedAst
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-

  $white+               ;
  "//".*                ;
  $digit+               { \_ s -> TokenNum (read s) }
  true                  { \_ s -> TokenBool True }
  false                 { \_ s -> TokenBool False }
  i64                   { \_ s -> TokenTypeI64 }
  i32                   { \_ s -> TokenTypeI32 }
  i16                   { \_ s -> TokenTypeI16 }
  i8                    { \_ s -> TokenTypeI8 }
  u64                   { \_ s -> TokenTypeU64 }
  u32                   { \_ s -> TokenTypeU32 }
  u16                   { \_ s -> TokenTypeU16 }
  u8                    { \_ s -> TokenTypeU8 }
  char                  { \_ s -> TokenTypeChar }
  bool                  { \_ s -> TokenTypeBool }
  return                { \_ s -> TokenReturn }
  while                 { \_ s -> TokenWhile }
  for                   { \_ s -> TokenFor }
  if                    { \_ s -> TokenIf }
  asm                    { \_ s -> TokenAsm }
  import                { \_ s -> TokenImport }
  module                { \_ s -> TokenModule }
  [A-Za-z][A-Za-z0-9_]* { \_ s -> TokenSym s }
  \#                    { \_ s -> TokenHash }
  \;                    { \_ s -> TokenSemi }
  \,                    { \_ s -> TokenComma }
  \.                    { \_ s -> TokenDot }
  \{                    { \_ s -> TokenLbrace }
  \}                    { \_ s -> TokenRbrace }
  \(                    { \_ s -> TokenLparen }
  \)                    { \_ s -> TokenRparen }
  \=                    { \_ s -> TokenAssign }
  \=>                   { \_ s -> TokenRefAssign }
  \+                    { \_ s -> TokenPlus }
  \-                    { \_ s -> TokenMinus }
  \*                    { \_ s -> TokenStar }
  \/                    { \_ s -> TokenDiv }
  \<=                   { \_ s -> TokenLte}
  \<                    { \_ s -> TokenLt}
  \>=                   { \_ s -> TokenGte}
  \>                    { \_ s -> TokenGt}
  \==                   { \_ s -> TokenEq}
  \!=                   { \_ s -> TokenNeq}
  \[                    { \_ s -> TokenLbrack}
  \]                    { \_ s -> TokenRbrack}
  \'[^']\'              { \_ s -> TokenChar (read s :: Char)}
  \'\\n\'               { \_ s -> TokenChar '\n'}
  \'\\t\'               { \_ s -> TokenChar '\t'}
  \"[^\"]*\"            { \_ s -> TokenString (read s :: String)}
  \`[[^\`]\n\t]*\`            { \_ s -> TokenLitAsm (takeWhile (/= '`') (tail s))}

{
data Token =
-- Reserved words
    TokenMain -- ^main
  | TokenReturn -- ^return
  | TokenWhile
  | TokenFor
  | TokenImport
  | TokenModule
  | TokenAsm
-- types
  | TokenTypeChar
  | TokenTypeString
  | TokenTypeBool
  | TokenTypeI64
  | TokenTypeI32
  | TokenTypeI16
  | TokenTypeI8
  | TokenTypeU64
  | TokenTypeU32
  | TokenTypeU16
  | TokenTypeU8
-- Literals
  | TokenNum Int       -- ^e.g. 12345
  | TokenBool Bool     -- ^true | false
  | TokenSym String    -- ^myvar
  | TokenChar Char     -- ^'c'
  | TokenString String -- ^"foo"
  | TokenLitAsm String -- ^`addq $8 %rsp`
-- Reserved Symbols
  | TokenSemi
  | TokenComma
  | TokenDot
  | TokenLbrace
  | TokenRbrace
  | TokenLparen
  | TokenRparen
  | TokenLbrack
  | TokenRbrack
-- Reserved operators
  | TokenAssign
  | TokenRefAssign
  | TokenHash
  | TokenPlus
  | TokenMinus
  | TokenStar
  | TokenDiv
  | TokenIf
  | TokenLt
  | TokenLte
  | TokenGt
  | TokenGte
  | TokenEq
  | TokenNeq
  deriving (Eq, Show)

scanTokens = alexScanTokens
}
