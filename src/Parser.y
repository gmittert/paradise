{
module Parser (
  parseProg,
) where

import Lexer
import Types
import Ast.ParsedAst

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
  : int main '(' ')' block { Prog $5}

block
  : '{' statements '}'                              {Block $2}

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
  | typ var '[' num ']' ';'           {SDecl (Name $2) (Arr $1 $4)}
  | while '(' expr ')' statement {SWhile $3 $5}
  | "if" '(' expr ')' statement {SIf $3 $5}
  | block                    {SBlock $1}
  | return expr ';'         {SReturn $2}

expr
  : expr '+' expr         {BOp Plus $1 $3}
  | expr '-' expr         {BOp Minus $1 $3}
  | expr '/' expr         {BOp Times $1 $3}
  | expr '*' expr         {BOp Div $1 $3}
  | expr '<' expr         {BOp Lt $1 $3}
  | expr "<=" expr        {BOp Lte $1 $3}
  | var '=' expr          {EAssign  (Name $1) $3}
  | expr '[' expr ']'     {BOp Access $1 $3}
  | num                  {Lit $1}
  | var                  {Var (Name $1)}
  | ch                   {Ch $1}

{
parseError :: [Token] -> Except String a
parseError [] = throwError "Unexpected end of input"
parseError a = throwError (show a)

parseProg :: String -> Either String Prog
parseProg input =
  let tokenStream = scanTokens input in
    runExcept (prog tokenStream)
}
