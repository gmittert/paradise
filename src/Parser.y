{
module Parser (
  parseModule
  , Token
  , ap2p
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
  type  { TokenType $$ }
  case  { TokenCase $$ }
  char  { TokenTypeChar $$ }
  void  { TokenTypeVoid $$ }
  str   { TokenTypeString $$ }
  varargs { TokenTypeVarargs $$ }
  ch    { TokenChar pos c }
  main  { TokenMain $$ }
  num   { TokenNum pos n }
  float { TokenFloat pos f }
  sym   { TokenSym pos v }
  tname { TokenTypeName pos v }
  string{ TokenString pos s }
  while { TokenWhile $$ }
  if    { TokenIf $$ }
  of    { TokenOf $$ }
  for   { TokenFor $$ }
  let   { TokenLet $$ }
  in    { TokenIn $$ }
  imprt { TokenImport $$ }
  foreign { TokenForeign $$ }
  mod   { TokenModule $$ }
  asm   { TokenAsm $$ }
  true  { TokenTrue $$ }
  false { TokenFalse $$ }
  '\\'  { TokenBSlash $$}
  '|'   { TokenPipe $$}
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
  "->"  { TokenTo $$ }

-- Parser Monad
%monad { Except [Token] } { (>>=) } { return }
%error { parseError }

-- Entry
%name prog

-- Operators
%left '+' '-'
%left '*' '/'
%%

module
  : modDecl imports rep(cfunc) rep(decl) {(\((Name var), posn) imprt cfn fn -> Module var imprt cfn fn posn) $1 $2 $3 $4}

modDecl
  : mod var {$2}

imports
  : {[]}
  | imports import {$2:$1}

import
  : imprt importPath {ModulePath $2}

cfunc
  : foreign var '(' commaList(typ) ')' ':' typ {CFunc (fst $2) $7 $4}

importPath
  : var {[toString (fst $1)]}
  | importPath '.' var {$1 ++ [toString (fst $3)]}

decl
  : func              {FuncDecl $1}
  | typeDec           {TypeDecl $1}

typeDec
  : type typeName '=' types {TypeDec (fst $2) $4}

types
  : typeName '(' commaList(typ) ')' {[(fst $1, $3)]}
  | types '|' typeName '(' commaList(typ) ')' {$1 ++ [(fst $3, $5)]}

typVar
  : typ var {($1, (fst $2))}

func
  : typ var '(' commaList(typVar) ')' '{' rep(statement) return expr ';' '}' {Func $1 (fst $2) $4 $7 $9 (snd $2)}
  | void var '(' commaList(typVar) ')' '{' rep(statement) '}' {Proc (fst $2) $4 $7 (snd $2)}
  | typ var '(' ')' '{' rep(statement) return expr ';' '}' {Func $1 (fst $2) [] $6 $8 (snd $2)}
  | void var '(' ')' '{' rep(statement) '}' {Proc (fst $2) [] $6 (snd $2)}

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
  | str                  {Lib.Types.Str arrAnyLen}
  | typ '[' ']'          {Arr $1 arrAnyLen}
  | typ '*'              {Ptr $1}
  | typeName             {UserType (fst $1)}

typAnnot
  : ':' typ              {$2}

statement
  : expr ';'              {SExpr $1 (posn ($1 :: Expr))}
  | let var opt(typAnnot) '=' expr ';'  {SDeclAssign (fst $2) $3 $5 (snd $2)}
  | while '(' expr ')' statement {SWhile $3 $5 (ap2p $1)}
  | if '(' expr ')' statement {SIf $3 $5 (posn ($3 :: Expr))}
  | for var in expr statement {ForEach (fst $2) $4 $5 (ap2p $1)}
  | '{' rep(statement) '}'      {SBlock $2 (ap2p $1)}
  | "[|" kexpr "|]" ';'   {Kernel $2 (ap2p $1)}
  | asm '(' string ':' opt(commaList(regConstr)) ':' opt(commaList(regConstr)) ':' opt(string) ':' opt(string) ')' ';' {Asm (strTok2S $3)  $5 $7 (fmap strTok2S $9) (fmap strTok2S $11) (ap2p $1)}

regConstr
  : string '(' var ')'   {(\(TokenString posn s) -> (s, fst $3)) $1}

-- 0 or 1 of
opt(c)
  : c                     {Just $1}
  |                       {Nothing}

-- A comma separated list of, e.g. 1,2,3,4
commaList(c)
  : c                         {[$1]}
  | commaList(c) ',' c        {$1 ++ [$3]}

-- 0 or more of
rep(c)
  :                           {[]}
  | rep(c) c                  {$1 ++ [$2]}

binds
  : var '=' expr           {[(fst $1, $3)]}
  | var '=' expr ';' binds {(fst $1, $3): $5}

expr
  : uop expr               {UOp $1 $2 (posn ($2 :: Expr))}
  | expr ':' typ           {UOp (Cast $3) $1 (posn ($1 :: Expr))}
  | expr bop expr          {BOp $2 $1 $3 (posn ($1 :: Expr))}
  -- | let binds in expr      {Let $2 $4}
  -- | '\\' varList '->' expr {Lambda $2 $4}
  -- The ArrAccess will get converted to an ArrStore in the Resolver if we
  -- decide we need an lval
  | expr '[' expr ']'      {BOp ArrAccess $1 $3 (posn ($1 :: Expr))}
  | '[' commaList(expr) ']'       {ArrLit $2 (ap2p $1)}
  | '[' listComp ']'       {ListComp $2 (ap2p $1)}
  | '[' ']'                {ArrLit [] (ap2p $1)}
  | num ':' numType        {case $3 of (Int sz s) -> (\(TokenNum posn n) -> Lit n sz s (ap2p posn)) $1; (Float sz) -> (\(TokenNum posn n) -> FLit (fromIntegral n) sz (ap2p posn)) $1}
  | num                    {(\(TokenNum posn n) -> Lit n IUnspec SUnspec (ap2p posn)) $1}
  | true                   {Lit 1 I1 Unsigned (ap2p $1)}
  | false                  {Lit 0 I1 Unsigned (ap2p $1)}
  | float                  {(\(TokenFloat posn n) -> FLit n FUnspec (ap2p posn)) $1}
  | float ':' numType      {case $3 of (Int sz s) -> error "Cast float as int"; (Float sz) -> (\(TokenFloat posn n) -> (FLit n sz (ap2p posn))) $1}
  | var '(' commaList(expr) ')'   {Call (fst $1) $3 (snd $1)}
  | var '(' ')'           {Call (fst $1) [] (snd $1)}
  | var                   {Var (fst $1) (snd $1)}
  | ch                    {(\(TokenChar posn c) -> Ch c (ap2p posn)) $1}
  | '(' expr ')'          {$2}
  | string                {(\(TokenString posn s) -> Ast.ParsedAst.Str s (ap2p posn)) $1}
  | typeName '(' commaList(expr) ')' {TypeConstr (fst $1) $3 (snd $1)}
  | case expr of commaList(patCase) {Case $2 $4 (ap2p $1)}

pat
  : ch                       {(\(TokenChar posn c) -> PCh c (ap2p posn)) $1}
  | float                    {(\(TokenFloat posn n) -> PFLit n FUnspec (ap2p posn)) $1}
  | num                      {(\(TokenNum posn n) -> PLit n IUnspec SUnspec (ap2p posn)) $1}
  | var                      {PVar (fst $1) (snd $1)}
  | typeName '(' commaList(pat) ')' {PTypeConstr (fst $1) $3 (snd $1)}

patCase
  : pat "->" expr            {($1, $3)}

uop
  : '#'  {Len}
  | '-'  {Neg}

listComp
  : expr for var in expr     {LFor  $1 (fst $3) $5 (posn ($1 :: Expr))}
-- Only [a,b .. c] or [a .. b] are legal, but we'll parse both here and reject
-- in the weeder. This gives a better error and also avoids a shift reduce
-- conflict
  | commaList(expr) ".." expr       {LRange $1 $3 (posn ((head $1) :: Expr))}

kexpr
  : var {KName (fst $1) (snd $1)}
  | kexpr kbop kexpr {KBOp $2 $1 $3 (posn ($1 :: KExpr))}

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

var
  : sym                   {(\(TokenSym pos n) -> (Name n, ap2p pos)) $1}

typeName
  : tname                 {(\(TokenTypeName pos n) -> (Name n, ap2p pos)) $1}

{
strTok2S :: Token -> String
strTok2S (TokenString posn s) = s
strTok2S a = error "strTok2S on non str tok"

ap2p :: AlexPosn -> Posn
ap2p (AlexPn _ l c) =  Posn l c

parseError :: [Token] -> Except [Token] a
parseError l = throwError l

parseModule :: String -> Either [Token] Module
parseModule input =
  let tokenStream = scanTokens input in
    runExcept (prog tokenStream)
}
