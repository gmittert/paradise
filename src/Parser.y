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
  asm   { TokenAsm }
  int   { TokenIntDec }
  char  { TokenCharDec }
  ch    { TokenChar $$ }
  main  { TokenMain }
  num   { TokenNum $$ }
  var   { TokenSym $$ }
  litasm{ TokenLitAsm $$ }
  while { TokenWhile }
  if    { TokenIf }
  imprt { TokenImport }
  mod   { TokenModule }
  ';'   { TokenSemi }
  ','   { TokenComma }
  '.'   { TokenDot}
  '+'   { TokenPlus }
  '-'   { TokenMinus }
  '*'   { TokenStar }
  '='   { TokenAssign }
  '=>'  { TokenRefAssign }
  '/'   { TokenDiv }
  '('   { TokenLparen }
  ')'   { TokenRparen }
  '{'   { TokenLbrace }
  '}'   { TokenRbrace }
  '['   { TokenLbrack}
  ']'   { TokenRbrack}
  '#'   { TokenHash}
  '<'   { TokenLt}
  "<="  { TokenLte}
  '>'   { TokenGt}
  ">="  { TokenGte}
  '=='  { TokenEq}
  "!="  { TokenNeq}

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
  : typ var '(' typArgs ')' '{' statements '}' {Func $1 (Name $2) (reverse $4) $7}
  | typ var '(' ')' '{' statements '}'      {Func $1 (Name $2) [] $6}
  {-| An asm function has no body
    |  e.g.
    |  asm int print(char c);
   -}
  | asm typ var '(' typArgs ')' '{' litasm '}' {AsmFunc $2 (Name $3) (reverse $5) $8}
  | asm typ var '(' ')' '{' litasm '}' {AsmFunc $2 (Name $3) [] $7}

funcs
  : func                 {[$1]}
  | funcs func           {$2:$1}

typArgs
  : typ var              {[($1, (Name $2))]}
  | typArgs ',' typ var  {($3, (Name $4)):$1}

statements
  : statement            {Statements' $1}
  | statements statement {Statements $1 $2}

typ
  : int                  {Int}
  | char                 {Char}

statement
  : expr ';'              {SExpr $1}
  | typ var ';'           {SDecl (Name $2) $1}
  | typ var '=' expr ';'     {SDeclAssign (Name $2) $1 $4}
  | typ var '[' num ']' '=' '{' exprList '}' ';'     {SDeclArr (Name $2) (Arr $1 $4) $8}
  | typ var '[' num ']' ';'           {SDeclArr (Name $2) (Arr $1 $4) []}
  | while '(' expr ')' statement {SWhile $3 $5}
  | if '(' expr ')' statement {SIf $3 $5}
  | '{' statements '}'   {SBlock $2}
  | return expr ';'         {SReturn $2}

expr
  : uop expr              {UOp $1 $2}
  | expr bop expr         {BOp $2 $1 $3}
  | var '=' expr          {EAssign  (Name $1) $3}
  | var '=>' expr         {ERefAssign  (Name $1) $3}
  | expr '[' expr ']' '=' expr {EAssignArr $1 $3 $6}
  | expr '[' expr ']'     {BOp Access $1 $3}
  | num                   {Lit $1}
  | var '(' exprList ')'  {Call (Name $1) (reverse $3)}
  | var '(' ')'           {Call (Name $1) []}
  | var                   {Var (Name $1)}
  | ch                    {Ch $1}
  | '(' expr ')'          {$2}

uop
  : '#'  {Len}

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

exprList
  : expr                  {[$1]}
  | exprList ',' expr     {$3 : $1}

{
parseError :: [Token] -> Except String a
parseError [] = throwError "Unexpected end of input"
parseError a = throwError (show a)

parseModule :: String -> Either String Module
parseModule input =
  let tokenStream = scanTokens input in
    runExcept (prog tokenStream)
}
