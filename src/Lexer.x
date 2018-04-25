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
  $digit+(\.$digit+)?   { \_ s -> if any (== '.') s then TokenFloat(read s) else TokenNum (read s)}
  true                  { \_ s -> TokenBool True }
  false                 { \_ s -> TokenBool False }
  f64                   { \_ s -> TokenTypeF64 }
  f32                   { \_ s -> TokenTypeF32 }
  i64                   { \_ s -> TokenTypeI64 }
  i32                   { \_ s -> TokenTypeI32 }
  i16                   { \_ s -> TokenTypeI16 }
  i8                    { \_ s -> TokenTypeI8 }
  u64                   { \_ s -> TokenTypeU64 }
  u32                   { \_ s -> TokenTypeU32 }
  u16                   { \_ s -> TokenTypeU16 }
  u8                    { \_ s -> TokenTypeU8 }
  char                  { \_ s -> TokenTypeChar }
  void                  { \_ s -> TokenTypeVoid }
  bool                  { \_ s -> TokenTypeBool }
  return                { \_ s -> TokenReturn }
  while                 { \_ s -> TokenWhile }
  for                   { \_ s -> TokenFor }
  if                    { \_ s -> TokenIf }
  let                    { \_ s -> TokenLet }
  in                    { \_ s -> TokenIn }
  import                { \_ s -> TokenImport }
  module                { \_ s -> TokenModule }
  C                     { \_ s -> TokenC }
  [A-Za-z][A-Za-z0-9_]* { \_ s -> TokenSym s }
  \#                    { \_ s -> TokenHash }
  \:                    { \_ s -> TokenColon}
  \;                    { \_ s -> TokenSemi }
  \,                    { \_ s -> TokenComma }
  \.                    { \_ s -> TokenDot }
  \{                    { \_ s -> TokenLbrace }
  \}                    { \_ s -> TokenRbrace }
  \(                    { \_ s -> TokenLparen }
  \)                    { \_ s -> TokenRparen }
  \[\|                   { \_ s -> TokenKernelLeft}
  \|\]                   { \_ s -> TokenKernelRight}
  \=                    { \_ s -> TokenAssign }
  \=>                   { \_ s -> TokenRefAssign }
  \+                    { \_ s -> TokenPlus }
  \-                    { \_ s -> TokenMinus }
  \*                    { \_ s -> TokenStar }
  \.\*                   { \_ s -> TokenElemMult }
  \.\+                   { \_ s -> TokenElemPlus }
  \/                    { \_ s -> TokenDiv }
  \<=                   { \_ s -> TokenLte}
  \->                   { \_ s -> TokenTo}
  \<                    { \_ s -> TokenLt}
  \>=                   { \_ s -> TokenGte}
  \>                    { \_ s -> TokenGt}
  \==                   { \_ s -> TokenEq}
  \!=                   { \_ s -> TokenNeq}
  \[                    { \_ s -> TokenLbrack}
  \]                    { \_ s -> TokenRbrack}
  \\                    { \_ s -> TokenBSlash}
  \'[^']\'              { \_ s -> TokenChar (read s :: Char)}
  \'\\n\'               { \_ s -> TokenChar '\n'}
  \'\\t\'               { \_ s -> TokenChar '\t'}
  \"[^\"]*\"            { \_ s -> TokenString (read s :: String)}
  \`[[^\`]\n\t]*\`      { \_ s -> TokenLitC (takeWhile (/= '`') (tail s))}

{
data Token =
-- Reserved words
    TokenMain -- ^main
  | TokenReturn -- ^return
  | TokenWhile
  | TokenFor
  | TokenImport
  | TokenModule
  | TokenC
-- types
  | TokenTypeChar
  | TokenTypeVoid
  | TokenTypeString
  | TokenTypeBool
  | TokenTypeF64
  | TokenTypeF32
  | TokenTypeI64
  | TokenTypeI32
  | TokenTypeI16
  | TokenTypeI8
  | TokenTypeU64
  | TokenTypeU32
  | TokenTypeU16
  | TokenTypeU8
-- Literals
  | TokenNum Int       -- ^e.g. 12345.2345
  | TokenFloat Double  -- ^e.g. 12345.2345
  | TokenBool Bool     -- ^true | false
  | TokenSym String    -- ^myvar
  | TokenChar Char     -- ^'c'
  | TokenString String -- ^"foo"
  | TokenLitC String -- ^`printf("%d", 5)`
-- Reserved Symbols
  | TokenColon
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
  | TokenLet
  | TokenIn
  | TokenLt
  | TokenLte
  | TokenGt
  | TokenGte
  | TokenEq
  | TokenNeq
  | TokenBSlash
  | TokenTo
-- Kernel Tokens
  | TokenKernelLeft
  | TokenKernelRight
  | TokenElemMult
  | TokenElemPlus
  deriving (Eq, Show)

scanTokens = alexScanTokens
}
