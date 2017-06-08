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
  ';'   { TokenSemi }
  ','   { TokenComma }
  '+'   { TokenPlus }
  '-'   { TokenMinus }
  '*'   { TokenStar }
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
  : int main block { Prog $3 emptyState}

block
  : '{' statements '}'                              {Block $2 emptyState}

args
  :                         {[]}
  | typ var ',' args        {(Arg $1 (Name $2)):$4}

statements
  : statement            {Statements' $1 emptyState}
  | statements statement {Statements $1 $2 emptyState}

typ
  : int                  {Int}
  | bool                 {Bool}
  | char                 {Char}

statement
  : expr ';'              {SExpr $1 emptyState}
  | var '=' expr ';'      {SAssign (Name $1) $3 emptyState}
  | typ var ';'           {SDecl (Name $2) $1 emptyState}
  | string var '=' str ';'{SDeclAssign (Name $2) (String (length $4)) (Str $4 emptyState) emptyState}
  | typ var '=' expr ';'  {SDeclAssign (Name $2) $1 $4 emptyState}
  | block             {SBlock $1 emptyState}

expr
  : var '+' expr         {Op Plus (Name $1) $3 emptyState}
  | var '-' expr         {Op Minus (Name $1) $3 emptyState}
  | num                  {Lit $1 emptyState}
  | var                  {Var (Name $1) emptyState}
  | str                  {Str $1 emptyState}
  | ch                   {Ch $1 emptyState}
  | true                 {Boolean True emptyState}
  | false                {Boolean False emptyState}

{
parseError :: [Token] -> Except String a
parseError [] = throwError "Unexpected end of input"
parseError a = throwError (show a)

parseProg :: String -> Either String Prog
parseProg input =
  let tokenStream = scanTokens input in
    runExcept (prog tokenStream)
}
