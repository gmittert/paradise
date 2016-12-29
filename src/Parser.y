{
module Parser (
  parseProg,
) where

import Lexer
import Syntax
import Types

import Control.Monad.Except
}

-- Lexer structure
%tokentype { Token }
-- Token Names
%token
  true  { TokenTrue }
  return{ TokenReturn }
  false { TokenFalse }
  int   { TokenIntDec }
  bool  { TokenBoolDec }
  char  { TokenCharDec }
  ch    { TokenChar $$ }
  main  { TokenMain }
  num   { TokenNum $$ }
  var   { TokenSym $$ }
  string{ TokenStringDec }
  str   { TokenString $$ }
  print { TokenPrint }
  ';'   { TokenSemi }
  '+'   { TokenPlus }
  '-'   { TokenMinus }
  '*'   { TokenMult }
  '='   { TokenAssign }
  '/'   { TokenDiv }
  '('   { TokenLparen }
  ')'   { TokenRparen }
  '{'   { TokenLbrace }
  '}'   { TokenRbrace }


-- Parser Monad
%monad { Except String } { (>>=) } { return }
%error { parseError }

-- Entry
%name prog

-- Operators
%left '+' '-'
%left '*' '/'
%%

prog
  : int main '(' ')' '{' statements return expr ';' '}' { Prog $6 $8 }

statements
  : statement            {Statements' $1}
  | statements statement {Statements $1 $2}

statement
  : expr ';'              {SExpr $1}
  | var '=' expr ';'      {SAssign (Name $1) $3}
  | print expr ';'        {SPrint  $2}
  | int var ';'           {SDecl (Name $2) Int }
  | bool var ';'          {SDecl (Name $2) Bool }
  | char var ';'          {SDecl (Name $2) Char }
  | string var '=' str ';'{SDeclAssign (Name $2) (String (length $4)) (Str $4)}
  | char var '=' expr ';' {SDeclAssign (Name $2) Char $4}
  | int var '=' expr ';'  {SDeclAssign (Name $2) Int $4}
  | bool var '=' expr ';' {SDeclAssign (Name $2) Bool $4}

expr
  : var '+' expr         {Op Plus (Name $1) $3}
  | var '-' expr         {Op Minus (Name $1) $3}
  | num                  {Lit $1}
  | var                  {Var (Name $1)}
  | str                  {Str $1}
  | ch                   {Ch $1}
  | true                 {Boolean True}
  | false                {Boolean False}

{
parseError :: [Token] -> Except String a
parseError [] = throwError "Unexpected end of input"
parseError a = throwError (show a)

parseProg :: String -> Either String Prog
parseProg input =
  let tokenStream = scanTokens input in
    runExcept (prog tokenStream)
}
