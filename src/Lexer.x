{
module Lexer (
 Token(..)
 , scanTokens
 , AlexPosn(..)
 , fmtTok
) where
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-

  $white+               ;
  "//".*                ;
  $digit+(\.$digit+)?   { \p s -> if any (== '.') s then TokenFloat p (read s) else TokenNum p (read s)}
  True                  { \p _ -> TokenTrue p}
  False                 { \p _ -> TokenFalse p}
  F64                   { \p _ -> TokenTypeF64 p}
  F32                   { \p _ -> TokenTypeF32 p}
  I64                   { \p _ -> TokenTypeI64 p}
  I32                   { \p _ -> TokenTypeI32 p}
  I16                   { \p _ -> TokenTypeI16 p}
  I8                    { \p _ -> TokenTypeI8 p}
  U64                   { \p _ -> TokenTypeU64 p}
  U32                   { \p _ -> TokenTypeU32 p}
  U16                   { \p _ -> TokenTypeU16 p}
  U8                    { \p _ -> TokenTypeU8 p}
  \.\.\.                { \p _ -> TokenTypeVarargs p}
  Char                  { \p _ -> TokenTypeChar p}
  Str                   { \p _ -> TokenTypeString p}
  Void                  { \p _ -> TokenTypeVoid p}
  Bool                  { \p _ -> TokenTypeBool p}
  [A-Z][A-Za-z0-9_]*    { \p s -> TokenTypeName p s }
  return                { \p _ -> TokenReturn p}
  while                 { \p _ -> TokenWhile p}
  for                   { \p _ -> TokenFor p}
  if                    { \p _ -> TokenIf p}
  let                   { \p _ -> TokenLet p}
  in                    { \p _ -> TokenIn p}
  import                { \p _ -> TokenImport p}
  foreign               { \p _ -> TokenForeign p}
  module                { \p _ -> TokenModule p}
  asm                   { \p _ -> TokenAsm p}
  case                  { \p _ -> TokenCase p}
  of                    { \p _ -> TokenOf p}
  type                  { \p _ -> TokenType p}
  [a-z_][A-Za-z0-9_]*   { \p s -> TokenSym p s }
  \|                    { \p _ -> TokenPipe p}
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
  | TokenAsm AlexPosn
  | TokenC AlexPosn
  | TokenCase AlexPosn
  | TokenOf AlexPosn
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
  | TokenType AlexPosn
-- Literals
  | TokenNum AlexPosn  Int       -- ^e.g. 12345.2345
  | TokenFloat AlexPosn  Double  -- ^e.g. 12345.2345
  | TokenTrue AlexPosn           -- ^true
  | TokenFalse AlexPosn          -- ^false
  | TokenSym AlexPosn  String    -- ^myvar
  | TokenTypeName AlexPosn  String    -- ^myvar
  | TokenChar AlexPosn  Char     -- ^'c'
  | TokenString AlexPosn  String -- ^"foo"
-- Reserved Symbols
  | TokenPipe AlexPosn
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
  deriving (Eq)

fmtPosn :: AlexPosn -> String
fmtPosn (AlexPn _ l c) = "Line: " ++ Prelude.show l ++ " Col: " ++ Prelude.show c
fmtTok :: Token -> String
fmtTok (TokenMain p) = "main (" ++ fmtPosn p ++ ")"
fmtTok (TokenReturn p) = "return (" ++ fmtPosn p ++ ")"
fmtTok (TokenWhile p) = "while (" ++ fmtPosn p ++ ")"
fmtTok (TokenFor p) = "for (" ++ fmtPosn p ++ ")"
fmtTok (TokenImport p) = "import (" ++ fmtPosn p ++ ")"
fmtTok (TokenForeign p) = "foreign (" ++ fmtPosn p ++ ")"
fmtTok (TokenModule p) = "module (" ++ fmtPosn p ++ ")"
fmtTok (TokenAsm p) = "asm (" ++ fmtPosn p ++ ")"
fmtTok (TokenC p) = "C (" ++ fmtPosn p ++ ")"
fmtTok (TokenType p) = "type (" ++ fmtPosn p ++ ")"
fmtTok (TokenTypeChar p) = "char (" ++ fmtPosn p ++ ")"
fmtTok (TokenTypeVoid p) = "void (" ++ fmtPosn p ++ ")"
fmtTok (TokenTypeString p) = "str (" ++ fmtPosn p ++ ")"
fmtTok (TokenTypeBool p) = "bool (" ++ fmtPosn p ++ ")"
fmtTok (TokenTypeF64 p) = "f64 (" ++ fmtPosn p ++ ")"
fmtTok (TokenTypeF32 p) = "f32 (" ++ fmtPosn p ++ ")"
fmtTok (TokenTypeI64 p) = "i64 (" ++ fmtPosn p ++ ")"
fmtTok (TokenTypeI32 p) = "i32 (" ++ fmtPosn p ++ ")"
fmtTok (TokenTypeI16 p) = "i16 (" ++ fmtPosn p ++ ")"
fmtTok (TokenTypeI8 p) = "i8 (" ++ fmtPosn p ++ ")"
fmtTok (TokenTypeU64 p) = "u64 (" ++ fmtPosn p ++ ")"
fmtTok (TokenTypeU32 p) = "u32 (" ++ fmtPosn p ++ ")"
fmtTok (TokenTypeU16 p) = "u16 (" ++ fmtPosn p ++ ")"
fmtTok (TokenTypeU8 p) = "u8 (" ++ fmtPosn p ++ ")"
fmtTok (TokenTypeVarargs p) = "... (" ++ fmtPosn p ++ ")"
fmtTok (TokenNum p i) = Prelude.show i ++ " (" ++ fmtPosn p ++ ")"
fmtTok (TokenFloat p d) = Prelude.show d ++ " (" ++ fmtPosn p ++ ")"
fmtTok (TokenTrue p) = "true (" ++ fmtPosn p ++ ")"
fmtTok (TokenFalse p) = "false (" ++ fmtPosn p ++ ")"
fmtTok (TokenChar p c) = Prelude.show c ++ " (" ++ fmtPosn p ++ ")"
fmtTok (TokenSym p s) = s ++ " (" ++ fmtPosn p ++ ")"
fmtTok (TokenTypeName p s) = s ++ " (" ++ fmtPosn p ++ ")"
fmtTok (TokenString p s) = "\"" ++ s ++ "\" (" ++ fmtPosn p ++ ")"
fmtTok (TokenPipe p) = "| (" ++ fmtPosn p ++ ")"
fmtTok (TokenColon p) = ": (" ++ fmtPosn p ++ ")"
fmtTok (TokenSemi p) = "; (" ++ fmtPosn p ++ ")"
fmtTok (TokenComma p) = ", (" ++ fmtPosn p ++ ")"
fmtTok (TokenDot p) = ". (" ++ fmtPosn p ++ ")"
fmtTok (TokenRange p) = ".. (" ++ fmtPosn p ++ ")"
fmtTok (TokenLbrace p) = "{ (" ++ fmtPosn p ++ ")"
fmtTok (TokenRbrace p) = "} (" ++ fmtPosn p ++ ")"
fmtTok (TokenLparen p) = "( (" ++ fmtPosn p ++ ")"
fmtTok (TokenRparen p) = ") (" ++ fmtPosn p ++ ")"
fmtTok (TokenLbrack p) = "[ (" ++ fmtPosn p ++ ")"
fmtTok (TokenRbrack p) = "] (" ++ fmtPosn p ++ ")"
fmtTok (TokenAssign p) = "= (" ++ fmtPosn p ++ ")"
fmtTok (TokenRefAssign p) = "<= (" ++ fmtPosn p ++ ")"
fmtTok (TokenHash p) = "# (" ++ fmtPosn p ++ ")"
fmtTok (TokenPlus p) = "+ (" ++ fmtPosn p ++ ")"
fmtTok (TokenMinus p) = "- (" ++ fmtPosn p ++ ")"
fmtTok (TokenStar p) = "* (" ++ fmtPosn p ++ ")"
fmtTok (TokenDiv p) = "/ (" ++ fmtPosn p ++ ")"
fmtTok (TokenIf p) = "if (" ++ fmtPosn p ++ ")"
fmtTok (TokenLet p) = "let (" ++ fmtPosn p ++ ")"
fmtTok (TokenIn p) = "in (" ++ fmtPosn p ++ ")"
fmtTok (TokenCase p) = "case (" ++ fmtPosn p ++ ")"
fmtTok (TokenOf p) = "of (" ++ fmtPosn p ++ ")"
fmtTok (TokenLt p) = "< (" ++ fmtPosn p ++ ")"
fmtTok (TokenLte p) = "<= (" ++ fmtPosn p ++ ")"
fmtTok (TokenGt p) = "> (" ++ fmtPosn p ++ ")"
fmtTok (TokenGte p) = ">= (" ++ fmtPosn p ++ ")"
fmtTok (TokenEq p) = "= (" ++ fmtPosn p ++ ")"
fmtTok (TokenNeq p) = "!= (" ++ fmtPosn p ++ ")"
fmtTok (TokenBSlash p) = "\\ (" ++ fmtPosn p ++ ")"
fmtTok (TokenTo p) = "-> (" ++ fmtPosn p ++ ")"
fmtTok (TokenKernelLeft p) = "[| (" ++ fmtPosn p ++ ")"
fmtTok (TokenKernelRight p) = "|] (" ++ fmtPosn p ++ ")"
fmtTok (TokenElemMult p) = ".* (" ++ fmtPosn p ++ ")"
fmtTok (TokenElemPlus p) = ".+ (" ++ fmtPosn p ++ ")"

scanTokens = alexScanTokens
}
