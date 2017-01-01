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
  ','   { TokenComma }
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
  : int main retblock { Prog $3 emptyTable}

retblock
  : '(' args ')' '{' statements return expr ';' '}' {Ret (Args $2) $5 $7 emptyTable}
  | '{' statements return expr ';' '}'              {Ret None $2 $4 emptyTable}

voidblock
  : '(' args ')' '{' statements '}'                 {Void (Args $2) $5 emptyTable}
  | '{' statements '}'                              {Void None $2 emptyTable}

args
  :                         {[]}
  | typ var ',' args        {(Arg $1 (Name $2)):$4}

statements
  : statement            {Statements' $1 emptyTable}
  | statements statement {Statements $1 $2 emptyTable}

typ
  : int                  {Int}
  | bool                 {Bool}
  | char                 {Char}

statement
  : expr ';'              {SExpr $1 emptyTable}
  | var '=' expr ';'      {SAssign (Name $1) $3 emptyTable}
  | print '(' expr ')' ';'{SPrint  $3 emptyTable}
  | typ var ';'           {SDecl (Name $2) $1 emptyTable}
  | string var '=' str ';'{SDeclAssign (Name $2) (String (length $4)) (Str $4 emptyTable) emptyTable}
  | typ var '=' expr ';'  {SDeclAssign (Name $2) $1 $4 emptyTable}
  | voidblock             {SBlock $1 emptyTable}

expr
  : var '+' expr         {Op Plus (Name $1) $3 emptyTable}
  | var '-' expr         {Op Minus (Name $1) $3 emptyTable}
  | num                  {Lit $1 emptyTable}
  | var                  {Var (Name $1) emptyTable}
  | str                  {Str $1 emptyTable}
  | ch                   {Ch $1 emptyTable}
  | true                 {Boolean True emptyTable}
  | false                {Boolean False emptyTable}
  | retblock             {EBlock $1 emptyTable}

{
parseError :: [Token] -> Except String a
parseError [] = throwError "Unexpected end of input"
parseError a = throwError (show a)

parseProg :: String -> Either String Prog
parseProg input =
  let tokenStream = scanTokens input in
    runExcept (prog tokenStream)
}
