{
module Lexer (
 Token(..)
 , scanTokens
 , AlexPosn(..)
) where
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-

  $white+               ;
  "//".*                ;
  $digit+(\.$digit+)?   { \p s -> if any (== '.') s then TokenFloat p (read s) else TokenNum p (read s)}
  true                  { \p s -> TokenTrue p}
  false                 { \p s -> TokenFalse p}
  f64                   { \p s -> TokenTypeF64 p}
  f32                   { \p s -> TokenTypeF32 p}
  i64                   { \p s -> TokenTypeI64 p}
  i32                   { \p s -> TokenTypeI32 p}
  i16                   { \p s -> TokenTypeI16 p}
  i8                    { \p s -> TokenTypeI8 p}
  u64                   { \p s -> TokenTypeU64 p}
  u32                   { \p s -> TokenTypeU32 p}
  u16                   { \p s -> TokenTypeU16 p}
  u8                    { \p s -> TokenTypeU8 p}
  char                  { \p s -> TokenTypeChar p}
  void                  { \p s -> TokenTypeVoid p}
  bool                  { \p s -> TokenTypeBool p}
  return                { \p s -> TokenReturn p}
  while                 { \p s -> TokenWhile p}
  for                   { \p s -> TokenFor p}
  if                    { \p s -> TokenIf p}
  let                   { \p s -> TokenLet p}
  in                    { \p s -> TokenIn p}
  import                { \p s -> TokenImport p}
  module                { \p s -> TokenModule p}
  C                     { \p s -> TokenC p}
  [A-Za-z][A-Za-z0-9_]* { \p s -> TokenSym p s }
  \#                    { \p s -> TokenHash p}
  \:                    { \p s -> TokenColon p}
  \;                    { \p s -> TokenSemi p}
  \,                    { \p s -> TokenComma p}
  \.                    { \p s -> TokenDot p}
  \.\.                  { \p s -> TokenRange p}
  \{                    { \p s -> TokenLbrace p}
  \}                    { \p s -> TokenRbrace p}
  \(                    { \p s -> TokenLparen p}
  \)                    { \p s -> TokenRparen p}
  \[\|                  { \p s -> TokenKernelLeft p}
  \|\]                  { \p s -> TokenKernelRight p}
  \=                    { \p s -> TokenAssign p}
  \=>                   { \p s -> TokenRefAssign p}
  \+                    { \p s -> TokenPlus p}
  \-                    { \p s -> TokenMinus p}
  \*                    { \p s -> TokenStar p}
  \.\*                  { \p s -> TokenElemMult p}
  \.\+                  { \p s -> TokenElemPlus p}
  \/                    { \p s -> TokenDiv p}
  \<=                   { \p s -> TokenLte p}
  \->                   { \p s -> TokenTo p}
  \<                    { \p s -> TokenLt p}
  \>=                   { \p s -> TokenGte p}
  \>                    { \p s -> TokenGt p}
  \==                   { \p s -> TokenEq p}
  \!=                   { \p s -> TokenNeq p}
  \[                    { \p s -> TokenLbrack p}
  \]                    { \p s -> TokenRbrack p}
  \\                    { \p s -> TokenBSlash p}
  \'[^']\'              { \p s -> TokenChar p (read s :: Char)}
  \'\\n\'               { \p s -> TokenChar p '\n'}
  \'\\t\'               { \p s -> TokenChar p '\t'}
  \"[^\"]*\"            { \p s -> TokenString p (read s :: String)}

{
data Token =
-- Reserved words
    TokenMain AlexPosn -- ^main
  | TokenReturn AlexPosn  -- ^return
  | TokenWhile AlexPosn
  | TokenFor AlexPosn
  | TokenImport AlexPosn
  | TokenModule AlexPosn
  | TokenC AlexPosn
-- types
  | TokenTypeChar AlexPosn
  | TokenTypeVoid AlexPosn
  | TokenTypeString AlexPosn
  | TokenTypeBool AlexPosn
  | TokenTypeF64 AlexPosn
  | TokenTypeF32 AlexPosn
  | TokenTypeI64 AlexPosn
  | TokenTypeI32 AlexPosn
  | TokenTypeI16 AlexPosn
  | TokenTypeI8 AlexPosn
  | TokenTypeU64 AlexPosn
  | TokenTypeU32 AlexPosn
  | TokenTypeU16 AlexPosn
  | TokenTypeU8 AlexPosn
-- Literals
  | TokenNum AlexPosn  Int       -- ^e.g. 12345.2345
  | TokenFloat AlexPosn  Double  -- ^e.g. 12345.2345
  | TokenTrue AlexPosn           -- ^true
  | TokenFalse AlexPosn          -- ^false
  | TokenSym AlexPosn  String    -- ^myvar
  | TokenChar AlexPosn  Char     -- ^'c'
  | TokenString AlexPosn  String -- ^"foo"
-- Reserved Symbols
  | TokenColon AlexPosn
  | TokenSemi AlexPosn
  | TokenComma AlexPosn
  | TokenDot AlexPosn            -- '.'
  | TokenRange AlexPosn          -- '..'
  | TokenLbrace AlexPosn         -- '{'
  | TokenRbrace AlexPosn         -- '}'
  | TokenLparen AlexPosn
  | TokenRparen AlexPosn
  | TokenLbrack AlexPosn
  | TokenRbrack AlexPosn
-- Reserved operators
  | TokenAssign AlexPosn
  | TokenRefAssign AlexPosn
  | TokenHash AlexPosn
  | TokenPlus AlexPosn
  | TokenMinus AlexPosn
  | TokenStar AlexPosn
  | TokenDiv AlexPosn
  | TokenIf AlexPosn
  | TokenLet AlexPosn
  | TokenIn AlexPosn
  | TokenLt AlexPosn
  | TokenLte AlexPosn
  | TokenGt AlexPosn
  | TokenGte AlexPosn
  | TokenEq AlexPosn
  | TokenNeq AlexPosn
  | TokenBSlash AlexPosn
  | TokenTo AlexPosn
-- Kernel Tokens
  | TokenKernelLeft AlexPosn
  | TokenKernelRight AlexPosn
  | TokenElemMult AlexPosn
  | TokenElemPlus AlexPosn
  deriving (Eq, Show)

scanTokens = alexScanTokens
}
