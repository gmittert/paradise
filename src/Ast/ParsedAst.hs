module Ast.ParsedAst where

import Lib.Types

data Module =
  Module String
         [ModulePath]
         [CFunc]
         [Function]
         Posn
  deriving (Eq, Ord, Show)

data Function
  = Func Type
         Name
         [(Type, Name)]
         [Statement]
         Expr
         Posn
  | Proc Name
         [(Type, Name)]
         [Statement]
         Posn
  deriving (Eq, Ord, Show)

data Arg =
  Arg Type
      Name
      Posn
  deriving (Eq, Ord, Show)

data Args
  = Args [Arg]
  | None
  deriving (Eq, Ord, Show)

data Statement
  = SExpr Expr
          Posn
  | SDecl Name
          Type
          Posn
  | SDeclAssign Name
                Type
                Expr
                Posn
  | SBlock [Statement]
           Posn
  | SWhile Expr
           Statement
           Posn
  | SIf Expr
        Statement
        Posn
  | ForEach Name
            Expr
            Statement
            Posn
  | Kernel KExpr
           Posn
  | Asm String
        (Maybe [(String, Name)])
        (Maybe [(String, Name)])
        (Maybe String)
        (Maybe String)
        Posn
  deriving (Eq, Ord, Show)

data Expr
  = BOp BinOp
        Expr
        Expr
        Posn
 -- | Lambda [Name] Expr
 -- | Let [(Name, Expr)] Expr
  | UOp UnOp
        Expr
        Posn
  | Lit Int
        IntSize
        SignType
        Posn
  | FLit Double
         FloatSize
         Posn
  | Var Name
        Posn
  | ArrLit [Expr]
           Posn
  | ListComp ListExpr
             Posn
  | Ch Char
       Posn
  | Call Name
         [Expr]
         Posn
  | Str String
        Posn
  deriving (Eq, Ord, Show)

-- Get the position from a statement
sposn :: Statement -> Posn
sposn (SExpr _ p) = p
sposn (SDecl _ _ p) = p
sposn (SDeclAssign _ _ _ p) = p
sposn (SBlock _ p) = p
sposn (SWhile _ _ p) = p
sposn (SIf _ _ p) = p
sposn (ForEach _ _ _ p) = p
sposn (Kernel _ p) = p
sposn (Asm _ _ _ _ _ p) = p

-- Get the position from an expression
eposn :: Expr -> Posn
eposn (BOp _ _ _ p) = p
eposn (UOp _ _ p) = p
eposn (Lit _ _ _ p) = p
eposn (FLit _ _ p) = p
eposn (Var _ p) = p
eposn (ArrLit _ p) = p
eposn (ListComp _ p) = p
eposn (Ch _ p) = p
eposn (Call _ _ p) = p
eposn (Ast.ParsedAst.Str _ p) = p

data KExpr
  = KBOp KBinOp
         KExpr
         KExpr
         Posn
  | KName Name
          Posn
  deriving (Eq, Ord, Show)

kposn :: KExpr -> Posn
kposn (KBOp _ _ _ p) = p
kposn (KName _ p) = p

data ListExpr
  = LFor Expr
         Name
         Expr
         Posn
  | LRange [Expr]
           Expr
           Posn
  deriving (Eq, Ord, Show)

leposn :: ListExpr -> Posn
leposn (LFor _ _ _ p) = p
leposn (LRange _ _ p) = p
