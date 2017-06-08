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
  return{ TokenReturn }
  int   { TokenIntDec }
  char  { TokenCharDec }
  ch    { TokenChar $$ }
  main  { TokenMain }
  num   { TokenNum $$ }
  var   { TokenSym $$ }
  while { TokenWhile }
  "if"  { TokenIf }
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
  '['   { TokenLbrack}
  ']'   { TokenRbrack}
  '<'   { TokenLt}
  "<="   { TokenLte}


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
  : int main '(' ')' block { Prog $5 emptyState}

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
  | char                 {Char}

statement
  : expr ';'              {SExpr $1 emptyState}
  | typ var ';'           {SDecl (Name $2) $1 emptyState}
  | typ var '=' expr ';'     {SDeclAssign (Name $2) $1 $4 emptyState}
  | typ var '[' num ']' ';'           {SDeclArr (Name $2) $1 $4 emptyState}
  | while '(' expr ')' statement {SWhile $3 $5 emptyState}
  | "if" '(' expr ')' statement {SIf $3 $5 emptyState}
  | block                    {SBlock $1 emptyState}
  | return expr ';'         {SReturn $2 emptyState}

expr
  : var '+' expr         {BOp Plus (Name $1) $3 emptyState}
  | var '-' expr         {BOp Minus (Name $1) $3 emptyState}
  | var '/' expr         {BOp Times (Name $1) $3 emptyState}
  | var '*' expr         {BOp Div (Name $1) $3 emptyState}
  | var '<' expr         {BOp Lt (Name $1) $3 emptyState}
  | var "<=" expr        {BOp Lte (Name $1) $3 emptyState}
  | expr '=' expr        {EAssign  $1 $3 emptyState}
  | var '[' expr ']'     {BOp Access (Name $1) $3 emptyState}
  | num                  {Lit $1 emptyState}
  | var                  {Var (Name $1) emptyState}
  | ch                   {Ch $1 emptyState}

{
parseError :: [Token] -> Except String a
parseError [] = throwError "Unexpected end of input"
parseError a = throwError (show a)

parseProg :: String -> Either String Prog
parseProg input =
  let tokenStream = scanTokens input in
    runExcept (prog tokenStream)
}
