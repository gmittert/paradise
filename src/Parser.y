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
  return{ TokenReturn $$ }
  C     { TokenC $$ }
  f64   { TokenTypeF64 $$ }
  f32   { TokenTypeF32 $$ }
  i64   { TokenTypeI64 $$ }
  i32   { TokenTypeI32 $$ }
  i16   { TokenTypeI16 $$ }
  i8    { TokenTypeI8 $$ }
  u64   { TokenTypeU64 $$ }
  u32   { TokenTypeU32 $$ }
  u16   { TokenTypeU16 $$ }
  u8    { TokenTypeU8 $$ }
  char  { TokenTypeChar $$ }
  void  { TokenTypeVoid $$ }
  varargs { TokenTypeVarargs $$ }
  ch    { TokenChar pos c }
  main  { TokenMain $$ }
  num   { TokenNum pos n }
  float { TokenFloat pos f }
  sym   { TokenSym pos v }
  str   { TokenString pos s }
  while { TokenWhile $$ }
  if    { TokenIf $$ }
  for   { TokenFor $$ }
  let   { TokenLet $$ }
  in    { TokenIn $$ }
  imprt { TokenImport $$ }
  foreign { TokenForeign $$ }
  mod   { TokenModule $$ }
  true  { TokenTrue $$ }
  false { TokenFalse $$ }
  '\\'  { TokenBSlash $$}
  ':'   { TokenColon $$}
  ';'   { TokenSemi $$}
  ','   { TokenComma $$}
  '.'   { TokenDot $$}
  ".."  { TokenRange $$ }
  '+'   { TokenPlus  $$ }
  '-'   { TokenMinus  $$ }
  '*'   { TokenStar  $$ }
  ".*"  { TokenElemMult $$ }
  ".+"  { TokenElemPlus $$ }
  '='   { TokenAssign  $$ }
  '=>'  { TokenRefAssign  $$ }
  '/'   { TokenDiv  $$ }
  '('   { TokenLparen  $$ }
  ')'   { TokenRparen  $$ }
  '{'   { TokenLbrace  $$ }
  '}'   { TokenRbrace  $$ }
  '['   { TokenLbrack $$ }
  ']'   { TokenRbrack $$ }
  "|]"  { TokenKernelRight $$ }
  "[|"  { TokenKernelLeft $$ }
  '#'   { TokenHash $$ }
  '<'   { TokenLt $$ }
  "<="  { TokenLte $$ }
  '>'   { TokenGt $$ }
  ">="  { TokenGte $$ }
  '=='  { TokenEq $$ }
  "!="  { TokenNeq $$ }
  '->'  { TokenTo $$ }

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
  : modDecl imports cfuncs funcs {(\((Name var), posn) imprt cfn fn -> Module var imprt cfn fn posn) $1 $2 $3 $4}

modDecl
  : mod var {$2}

imports
  : {[]}
  | imports import {$2:$1}

import
  : imprt importPath {ModulePath $2}

cfunc
  : foreign var '(' typs ')' ':' typ {CFunc (fst $2) $7 $4}

cfuncs
  :                   {[]}
  | cfuncs cfunc      {$1 ++ [$2]}

importPath
  : var {[toString (fst $1)]}
  | importPath '.' var {$1 ++ [toString (fst $3)]}

func
  : typ var '(' typArgs ')' '{' statements return expr ';' '}' {Func $1 (fst $2) $4 $7 $9 (snd $2)}
  | void var '(' typArgs ')' '{' statements '}' {Proc (fst $2) $4 $7 (snd $2)}
  | typ var '(' ')' '{' statements return expr ';' '}' {Func $1 (fst $2) [] $6 $8 (snd $2)}
  | void var '(' ')' '{' statements '}' {Proc (fst $2) [] $6 (snd $2)}

funcs
  :                      {[]}
  | funcs func           {$1 ++ [$2]}

typArgs
  : typ var              {[($1, (fst $2))]}
  | typArgs ',' typ var  {$1 ++[($3, (fst $4))]}

statements
  :                      {[]}
  | statements statement {$1 ++ [$2]}

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
  | varargs              {Varargs}
  | typ '[' ']'          {Arr $1 arrAnyLen}

typs
  : typ                  {[$1]}
  | typs ',' typ         {$1 ++ [$3]}

statement
  : expr ';'              {SExpr $1 (eposn $1)}
  | typ var ';'           {SDecl (fst $2) $1 (snd $2)}
  | typ var '=' expr ';'  {SDeclAssign (fst $2) $1 $4 (snd $2)}
  | while '(' expr ')' statement {SWhile $3 $5 (ap2p $1)}
  | if '(' expr ')' statement {SIf $3 $5 (eposn $3)}
  | for var in expr statement {ForEach (fst $2) $4 $5 (ap2p $1)}
  | '{' statements '}'      {SBlock $2 (ap2p $1)}
  | "[|" kexpr "|]" ';'   {Kernel $2 (ap2p $1)}

binds
  : var '=' expr           {[(fst $1, $3)]}
  | var '=' expr ';' binds {(fst $1, $3): $5}

expr
  : uop expr               {UOp $1 $2 (eposn $2)}
  | expr bop expr          {BOp $2 $1 $3 (eposn $1)}
  -- | let binds in expr      {Let $2 $4}
  -- | '\\' varList '->' expr {Lambda $2 $4}
  -- The ArrAccess will get converted to an ArrStore in the Resolver if we
  -- decide we need an lval
  | expr '[' expr ']'      {BOp ArrAccess $1 $3 (eposn $1)}
  | '[' exprList ']'       {ArrLit $2 (ap2p $1)}
  | '[' listComp ']'       {ListComp $2 (ap2p $1)}
  | '[' ']'                {ArrLit [] (ap2p $1)}
  | num ':' numType        {case $3 of (Int sz s) -> (\(TokenNum posn n) -> Lit n sz s (ap2p posn)) $1; (Float sz) -> (\(TokenNum posn n) -> FLit (fromIntegral n) sz (ap2p posn)) $1}
  | num                    {(\(TokenNum posn n) -> Lit n IUnspec SUnspec (ap2p posn)) $1}
  | true                   {Lit 1 I1 Unsigned (ap2p $1)}
  | false                  {Lit 0 I1 Unsigned (ap2p $1)}
  | float                  {(\(TokenFloat posn n) -> FLit n FUnspec (ap2p posn)) $1}
  | float ':' numType      {case $3 of (Int sz s) -> error "Cast float as int"; (Float sz) -> (\(TokenFloat posn n) -> (FLit n sz (ap2p posn))) $1}
  | var '(' exprList ')'   {Call (fst $1) $3 (snd $1)}
  | var '(' ')'           {Call (fst $1) [] (snd $1)}
  | var                   {Var (fst $1) (snd $1)}
  | ch                    {(\(TokenChar posn c) -> Ch c (ap2p posn)) $1}
  | '(' expr ')'          {$2}
  | str                   {(\(TokenString posn s) -> Ast.ParsedAst.Str s (ap2p posn)) $1}

uop
  : '#'  {Len}
  | '-'  {Neg}

listComp
  : expr for var in expr     {LFor  $1 (fst $3) $5 (eposn $1)}
-- Only [a,b .. c] or [a .. b] are, but we'll parse both here and reject
-- in the weeder. This gives a better error and also avoids a shift reduce
-- conflict
  | exprList ".." expr       {LRange $1 $3 (eposn (head $1))}

kexpr
  : var {KName (fst $1) (snd $1)}
  | kexpr kbop kexpr {KBOp $2 $1 $3 (kposn $1)}

bop
  : '+'  { Plus   }
  | '-'  { Minus  }
  | '/'  { Div    }
  | '*'  { Times  }
  | '<'  { Lt     }
  | "<=" { Lte    }
  | '>'  { Gt     }
  | ">=" { Gte    }
  | '==' { Eq     }
  | "!=" { Neq    }
  | '='  { Assign }

kbop
  : ".+" { ElemPlus }
  | ".*" { ElemMult }
  | '*'  { MMult    }
  | '='  { KAssign  }

exprList
  : expr                  {[$1]}
  | exprList ',' expr     {$1 ++ [$3]}

varList
  : var                   {[fst $1]}
  | varList var           {$1 ++ [(fst $2)]}

var
  : sym                   {(\(TokenSym pos n) -> (Name n, ap2p pos)) $1}
{
ap2p :: AlexPosn -> Posn
ap2p (AlexPn _ l c) =  Posn l c

parseError :: [Token] -> Except String a
parseError [] = throwError "Unexpected end of input"
parseError a = throwError (show a)

parseModule :: String -> Either String Module
parseModule input =
  let tokenStream = scanTokens input in
    runExcept (prog tokenStream)
}
