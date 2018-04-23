module Ast.CAst where

import Lib.Types
import Lib.Format
import GenCState
import qualified Data.Set as S
import Control.Monad.State.Lazy

includes :: String
includes = "#include <stdio.h>\n\
           \#include <stdint.h>\n\
           \#include <stdlib.h>\n\
           \"
data Prog = Prog [Statement] [Function]
  deriving (Eq, Ord)
instance Show Prog where
  show (Prog stms f) = includes ++ delimWith "\n" stms ++ delimWith "\n" f

data CType
  = Int8 | UInt8
  | Int16 | UInt16
  | Int32 | UInt32
  | Int64 | UInt64
  | Float | Double
  | Int
  | Char
  | Void
  | Bool
  | Ptr CType
  | Struct String
  deriving (Eq, Ord)
instance Show CType where
  show Int8 = "int8_t"
  show UInt8 = "uint8_t"
  show Int16 = "int16_t"
  show UInt16 = "uint16_t"
  show Int32 = "int32_t"
  show UInt32 = "uint32_t"
  show Int64 = "int64_t"
  show UInt64 = "uint64_t"
  show Ast.CAst.Int = "int"
  show Float = "float"
  show Double = "double"
  show Ast.CAst.Void = "void"
  show Ast.CAst.Bool = "bool"
  show Ast.CAst.Char = "char"
  show (Ptr t) = show t ++ "*"
  show (Struct s) = "struct " ++ s

toCType :: Type -> GenC Statement CType
toCType (Lib.Types.Int I8 Signed) = return Int8
toCType (Lib.Types.Int I8 Unsigned) = return UInt8
toCType (Lib.Types.Int I16 Signed) = return Int16
toCType (Lib.Types.Int I16 Unsigned) = return UInt16
toCType (Lib.Types.Int I32 Signed) = return Int32
toCType (Lib.Types.Int I32 Unsigned) = return UInt32
toCType (Lib.Types.Int I64 Signed) = return Int64
toCType (Lib.Types.Int I64 Unsigned) = return UInt64
toCType Lib.Types.Void = return Ast.CAst.Void
toCType Lib.Types.Bool = return Ast.CAst.Bool
toCType Lib.Types.Char = return Ast.CAst.Char
toCType (Arr t) = do
  tpe' <- toCType t
  modify $ \s -> s{defs = S.insert (makeArrDec tpe') (defs s)}
  return $ makeArr tpe'
toCType Str = return $ Ptr Ast.CAst.Char

makeArrDec :: CType -> Statement
makeArrDec t = StructDef (show t ++ "_arr") [(Ptr t, "data"), (UInt32, "len")]

makeArr :: CType -> CType
makeArr t = Struct (show t ++ "_arr")

data Function
  = Func CType QualifiedName [(CType, Name)] [Statement] Expr
  | Proc QualifiedName [(CType, Name)] [Statement]
  | CFunc String
  deriving (Eq, Ord)

formatParams :: [(CType, Name)] -> String
formatParams params = tail $ concatMap (\(tpe,name) -> ", " ++ show tpe ++ " " ++ show name) params ++ " "

instance Show Function where
  show (Func tpe name params stmnts ret) =
    show tpe ++ " " ++ show name ++ "( " ++ formatParams params ++ ")" ++ "{\n"  ++ concatMap show stmnts ++ "return " ++ show ret ++ ";\n}"
  show (Proc name params stmnts) =
    "void " ++ show name ++ "( " ++ formatParams params ++ ")" ++ "{"  ++ concatMap (\x -> show x ++ "\n") stmnts ++ "}"
  show (CFunc c) = c

data Statement
  = SExpr Expr
  | SDecl Name CType
  | SDeclAssign Name CType Expr
  | SBlock [Statement]
  | SWhile Expr Statement
  | For Expr Expr Expr Statement
  | SIf Expr Statement
  | StructDef String [(CType, String)]
  | FDeclare Function
  deriving (Eq, Ord)
instance Show Statement where
  show (SExpr e) = show e ++ ";\n"
  show (SBlock s) = "{\n" ++ concatMap show s ++ "\n}"
  show (SDecl name tpe) = show tpe ++ " " ++ show name ++ ";\n"
  show (SDeclAssign name tpe expr) = show tpe ++ " " ++ show name ++ " = " ++ show expr ++ ";\n"
  show (SWhile e stmnt) = "while (" ++ show e ++ ")\n" ++ show stmnt
  show (For init cmp mod stmnt) = "for (" ++ show init ++ ";" ++ show cmp ++ ";" ++ show mod ++ ")\n" ++ show stmnt
  show (SIf e stmnt) = "if (" ++ show e ++ ")\n" ++ show stmnt
  show (StructDef s fields) = "struct " ++ s ++ "{\n" ++ concatMap (\(x,y) -> (show x ++ " " ++ y ++ ";\n")) fields ++ "};\n"
  show (FDeclare f) = case f of
    (Func tpe name params _ _) -> show tpe ++ " " ++ show name ++ "( " ++ formatParams params ++ ");\n"
    (Proc name params _) -> "void " ++ show name ++ "( " ++ formatParams params ++ ");\n"
    (CFunc c) -> ""

data Expr
 = BOp BinOp Expr Expr
 | EAssign Name Expr
 | EAssignArr Expr Expr Expr
 | Field Expr String
 | EAssignF Expr String Expr
 | UOp UnOp Expr
 | Lit Int
 | Var Name
 | FuncName QualifiedName
 | Ch Char
 | Call QualifiedName [Expr]
 | Malloc Expr
 | Free String
  deriving (Eq, Ord)
instance Show Expr where
  show (BOp Access e1 e2) = show e1 ++ ".data[" ++ show e2 ++ "]"
  show (BOp op e1 e2) = show e1 ++ " " ++ show op ++ " " ++ show e2
  show (EAssign name expr) = show name ++ " = " ++ show expr
  show (EAssignArr e1 e2 e3) = show e1 ++ "[" ++ show e2 ++ "] = " ++ show e3
  show (UOp Len e1) = show e1 ++ ".len"
  show (UOp op e1) = show op ++ " " ++ show e1
  show (Lit i) = show i
  show (Var name) = show name
  show (FuncName name) = show name
  show (Ch char) = show char
  show (Field exp f) = show exp ++ "." ++ f
  show (EAssignF exp1 f exp2) = show exp1 ++ "." ++ f ++ " = " ++ show exp2
  show (Call name exprs) = show name ++ "(" ++ commaList exprs++ ")"
  show (Malloc expr) = "malloc(" ++ show expr ++ ")"
  show (Free name) = "free(" ++ name ++ ")"

