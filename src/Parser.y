{
module Parser (
  parseModule,
) where

import Lexer
import Lib.Types
import Ast.ParsedAst

import Control.Monad.Except
}

-- Lexer structure
%tokentype { Token }
-- Token Names
%token
  return{ TokenReturn }
  C     { TokenC }
  f64   { TokenTypeF64 }
  f32   { TokenTypeF32 }
  i64   { TokenTypeI64 }
  i32   { TokenTypeI32 }
  i16   { TokenTypeI16 }
  i8    { TokenTypeI8 }
  u64   { TokenTypeU64 }
  u32   { TokenTypeU32 }
  u16   { TokenTypeU16 }
  u8    { TokenTypeU8 }
  char  { TokenTypeChar }
  void  { TokenTypeVoid }
  ch    { TokenChar $$ }
  main  { TokenMain }
  num   { TokenNum $$ }
  float { TokenFloat $$ }
  var   { TokenSym $$ }
  litC { TokenLitC $$ }
  str   { TokenString $$ }
  while { TokenWhile }
  if    { TokenIf }
  for   { TokenFor }
  let   { TokenLet }
  in    { TokenIn }
  imprt { TokenImport }
  mod   { TokenModule }
  '\\'  { TokenBSlash}
  ':'   { TokenColon}
  ';'   { TokenSemi }
  ','   { TokenComma }
  '.'   { TokenDot}
  '+'   { TokenPlus }
  '-'   { TokenMinus }
  '*'   { TokenStar }
  ".*"  { TokenElemMult}
  ".+"  { TokenElemPlus}
  '='   { TokenAssign }
  '=>'  { TokenRefAssign }
  '/'   { TokenDiv }
  '('   { TokenLparen }
  ')'   { TokenRparen }
  '{'   { TokenLbrace }
  '}'   { TokenRbrace }
  '['   { TokenLbrack}
  ']'   { TokenRbrack}
  "|]"  { TokenKernelRight}
  "[|"  { TokenKernelLeft}
  '#'   { TokenHash}
  '<'   { TokenLt}
  "<="  { TokenLte}
  '>'   { TokenGt}
  ">="  { TokenGte}
  '=='  { TokenEq}
  "!="  { TokenNeq}
  '->'  { TokenTo}

-- Parser Monad
%monad { Except String } { (>>=) } { return }
%error { parseError }

-- Entry
%name prog

-- Operators
%left '+' '-'
%left '*' '/'
%%

module
  : modDecl imports funcs {Module $1 $2 $3}

modDecl
  : mod var {$2}

imports
  : {[]}
  | imports import {$2:$1}

import
  : imprt importPath {ModulePath $2}

importPath
  : var {[$1]}
  | importPath '.' importPath {$1 ++ $3}

func
  : typ var '(' typArgs ')' '{' statements return expr ';' '}' {Func $1 (Name $2) (reverse $4) $7 $9}
  | void var '(' typArgs ')' '{' statements '}' {Proc (Name $2) (reverse $4) $7}
  | typ var '(' ')' '{' statements return expr ';' '}' {Func $1 (Name $2) [] $6 $8}
  | void var '(' ')' '{' statements '}' {Proc (Name $2) [] $6}
  {-| A C function has no body
    |  e.g.
    |  C int print(char c);
   -}
  | C typ var '(' typArgs ')' '{' litC '}' {CFunc $2 (Name $3) (reverse $5) $8}
  | C typ var '(' ')' '{' litC '}' {CFunc $2 (Name $3) [] $7}

funcs
  : func                 {[$1]}
  | funcs func           {$2:$1}

typArgs
  : typ var              {[($1, (Name $2))]}
  | typArgs ',' typ var  {($3, (Name $4)):$1}

statements
  :                      {[]}
  | statement statements {$1:$2}

numType
  : i64                  {Int I64 Signed}
  | i32                  {Int I32 Signed}
  | i16                  {Int I16 Signed}
  | i8                   {Int I8 Signed}
  | u64                  {Int I64 Unsigned}
  | u32                  {Int I32 Unsigned}
  | u16                  {Int I16 Unsigned}
  | u8                   {Int I8 Unsigned}
  | f64                  {Float F64}
  | f32                  {Float F32}

typ
  : numType              {$1}
  | char                 {Char}
  | typ '[' ']'          {Arr $1}

statement
  : expr ';'              {SExpr $1}
  | typ var ';'           {SDecl (Name $2) $1}
  | typ var '=' expr ';'  {SDeclAssign (Name $2) $1 $4}
  | while '(' expr ')' statement {SWhile $3 $5}
  | if '(' expr ')' statement {SIf $3 $5}
  | for var in expr statement {ForEach (Name $2) $4 $5}
  | '{' statements '}'      {SBlock $2}
  | "[|" kexpr "|]" ';'   {Kernel $2}

binds
  : var '=' expr           {[((Name $1), $3)]}
  | var '=' expr ';' binds {((Name $1), $3): $5}

expr
  : uop expr              {UOp $1 $2}
  | expr bop expr         {BOp $2 $1 $3}
  | let binds in expr     {Let $2 $4}
  | '\\' varList '->' expr {Lambda $2 $4}
  | var '=' expr          {EAssign  (Name $1) $3}
  | expr '[' expr ']' '=' expr {EAssignArr $1 $3 $6}
  | expr '[' expr ']'     {BOp Access $1 $3}
  | '[' exprList ']'      {ArrLit $2}
  | '[' ']'               {ArrLit []}
  | num ':' numType       {case $3 of (Int sz s) -> Lit $1 sz s; (Float sz) -> (FLit (fromIntegral $1) sz)}
  | num                   {Lit $1 IUnspec SUnspec}
  | float                 {FLit $1 FUnspec}
  | float ':' numType     {case $3 of (Int sz s) -> error "Cast float as int"; (Float sz) -> (FLit $1 sz)}
  | var '(' exprList ')'  {Call (Name $1) (reverse $3)}
  | var '(' ')'           {Call (Name $1) []}
  | var                   {Var (Name $1)}
  | ch                    {Ch $1}
  | '(' expr ')'          {$2}
  | str                   {Ast.ParsedAst.Str (reverse $1)}

uop
  : '#'  {Len}
  | '-'  {Neg}

kexpr
  : var {KName (Name $1)}
  | kexpr kbop kexpr {KBOp $2 $1 $3}

bop
  : '+'  { Plus }
  | '-'  { Minus }
  | '/'  { Div   }
  | '*'  { Times }
  | '<'  { Lt    }
  | "<=" { Lte   }
  | '>'  { Gt    }
  | ">=" { Gte   }
  | '==' { Eq    }
  | "!=" { Neq   }

kbop
  : ".+"  { ElemPlus }
  | ".*"  { ElemMult}
  | '*'  { MMult}
  | '='  { KAssign    }

exprList
  : expr                  {[$1]}
  | exprList ',' expr     {$3 : $1}

varList
  : var                   {[Name $1]}
  | var varList           {(Name $1) : $2}

{
parseError :: [Token] -> Except String a
parseError [] = throwError "Unexpected end of input"
parseError a = throwError (show a)

parseModule :: String -> Either String Module
parseModule input =
  let tokenStream = scanTokens input in
    runExcept (prog tokenStream)
}
