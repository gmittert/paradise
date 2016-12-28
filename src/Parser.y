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
  int   { TokenInt }
  main  { TokenMain }
  num   { TokenNum $$ }
  var   { TokenSym $$ }
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
  : int main '(' ')' '{' decls statements return expr ';' '}' { Prog $6 $7 $9 }

decls
  : int var ';'       { Decls' (Name $2) Int }
  | decls int var ';' { Decls $1 (Name $3) Int }

statements
  : statement            {Statements' $1}
  | statements statement {Statements $1 $2}

statement
  : expr ';'             {SExpr $1}
  | var '=' expr ';'     {SAssign (Name $1) $3}
  | print expr ';'       {SPrint  $2}

expr
  : var '+' expr         {Op Plus (Name $1) $3}
  | var '-' expr         {Op Minus (Name $1) $3}
  | num                  {Lit $1}
  | var                  {Var (Name $1)}

{
parseError :: [Token] -> Except String a
parseError (l:ls) = throwError (show (l:ls))
parseError [] = throwError "Unexpected end of input"

parseProg :: String -> Either String Prog
parseProg input =
  let tokenStream = scanTokens input in
    runExcept (prog tokenStream)
}
