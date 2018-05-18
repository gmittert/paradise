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
  true                  { \p _ -> TokenTrue p}
  false                 { \p _ -> TokenFalse p}
  f64                   { \p _ -> TokenTypeF64 p}
  f32                   { \p _ -> TokenTypeF32 p}
  i64                   { \p _ -> TokenTypeI64 p}
  i32                   { \p _ -> TokenTypeI32 p}
  i16                   { \p _ -> TokenTypeI16 p}
  i8                    { \p _ -> TokenTypeI8 p}
  u64                   { \p _ -> TokenTypeU64 p}
  u32                   { \p _ -> TokenTypeU32 p}
  u16                   { \p _ -> TokenTypeU16 p}
  u8                    { \p _ -> TokenTypeU8 p}
  \.\.\.                { \p _ -> TokenTypeVarargs p}
  char                  { \p _ -> TokenTypeChar p}
  void                  { \p _ -> TokenTypeVoid p}
  bool                  { \p _ -> TokenTypeBool p}
  return                { \p _ -> TokenReturn p}
  while                 { \p _ -> TokenWhile p}
  for                   { \p _ -> TokenFor p}
  if                    { \p _ -> TokenIf p}
  let                   { \p _ -> TokenLet p}
  in                    { \p _ -> TokenIn p}
  import                { \p _ -> TokenImport p}
  foreign               { \p _ -> TokenForeign p}
  module                { \p _ -> TokenModule p}
  [A-Za-z][A-Za-z0-9_]* { \p s -> TokenSym p s }
  \#                    { \p _ -> TokenHash p}
  \:                    { \p _ -> TokenColon p}
  \;                    { \p _ -> TokenSemi p}
  \,                    { \p _ -> TokenComma p}
  \.                    { \p _ -> TokenDot p}
  \.\.                  { \p _ -> TokenRange p}
  \{                    { \p _ -> TokenLbrace p}
  \}                    { \p _ -> TokenRbrace p}
  \(                    { \p _ -> TokenLparen p}
  \)                    { \p _ -> TokenRparen p}
  \[\|                  { \p _ -> TokenKernelLeft p}
  \|\]                  { \p _ -> TokenKernelRight p}
  \=                    { \p _ -> TokenAssign p}
  \=>                   { \p _ -> TokenRefAssign p}
  \+                    { \p _ -> TokenPlus p}
  \-                    { \p _ -> TokenMinus p}
  \*                    { \p _ -> TokenStar p}
  \.\*                  { \p _ -> TokenElemMult p}
  \.\+                  { \p _ -> TokenElemPlus p}
  \/                    { \p _ -> TokenDiv p}
  \<=                   { \p _ -> TokenLte p}
  \->                   { \p _ -> TokenTo p}
  \<                    { \p _ -> TokenLt p}
  \>=                   { \p _ -> TokenGte p}
  \>                    { \p _ -> TokenGt p}
  \==                   { \p _ -> TokenEq p}
  \!=                   { \p _ -> TokenNeq p}
  \[                    { \p _ -> TokenLbrack p}
  \]                    { \p _ -> TokenRbrack p}
  \\                    { \p _ -> TokenBSlash p}
  \'[^']\'              { \p s -> TokenChar p (read s :: Char)}
  \'\\n\'               { \p _ -> TokenChar p '\n'}
  \'\\t\'               { \p _ -> TokenChar p '\t'}
  \"[^\"]*\"            { \p s -> TokenString p (read s :: String)}

{
data Token =
-- Reserved words
    TokenMain AlexPosn -- ^main
  | TokenReturn AlexPosn  -- ^return
  | TokenWhile AlexPosn
  | TokenFor AlexPosn
  | TokenImport AlexPosn
  | TokenForeign AlexPosn
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
  | TokenTypeVarargs AlexPosn
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
