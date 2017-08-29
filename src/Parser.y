{
module Parser (
  parseProg,
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
  : funcs {Prog $1}

func
  : typ var '(' typArgs ')' '{' statements '}' {Func $1 (Name $2) (reverse $4) $7}
  | typ var '(' ')' '{' statements '}'      {Func $1 (Name $2) [] $6}

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
  | typ var '[' num ']' '=' expr ';'     {SDeclAssign (Name $2) (Arr $1 $4) $7}
  | typ var '[' num ']' ';'           {SDecl (Name $2) (Arr $1 $4)}
  | while '(' expr ')' statement {SWhile $3 $5}
  | "if" '(' expr ')' statement {SIf $3 $5}
  | '{' statements '}'   {SBlock $2}
  | return expr ';'         {SReturn $2}

expr
  : expr '+' expr         {BOp Plus $1 $3}
  | expr '-' expr         {BOp Minus $1 $3}
  | expr '/' expr         {BOp Div $1 $3}
  | expr '*' expr         {BOp Times $1 $3}
  | expr '<' expr         {BOp Lt $1 $3}
  | expr "<=" expr        {BOp Lte $1 $3}
  | '{' exprList '}'      {EArr $2}
  | var '=' expr          {EAssign  (Name $1) $3}
  | expr '[' expr ']' '=' expr {EAssignArr $1 $3 $6}
  | expr '[' expr ']'     {BOp Access $1 $3}
  | num                   {Lit $1}
  | var '(' exprList ')'  {Call (Name $1) (reverse $3)}
  | var '(' ')'  {Call (Name $1) []}
  | var                   {Var (Name $1)}
  | ch                    {Ch $1}
  | '(' expr ')'          {$2}

exprList
  : expr                  {[$1]}
  | exprList ',' expr     {$3 : $1}

{
parseError :: [Token] -> Except String a
parseError [] = throwError "Unexpected end of input"
parseError a = throwError (show a)

parseProg :: String -> Either String Prog
parseProg input =
  let tokenStream = scanTokens input in
    runExcept (prog tokenStream)
}
