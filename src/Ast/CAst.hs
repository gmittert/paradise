module Ast.CAst where
import Lib.Types


newtype Prog = Prog [Function]
  deriving (Eq, Ord)
instance Show Prog where
  show (Prog f) = concatMap (\x -> show x ++ "\n") f

data CType
  = Int8 | UInt8
  | Int16 | UInt16
  | Int32 | UInt32
  | Int64 | UInt64
  | Float | Double
  | Char
  | Void
  | Bool
  | Ptr CType
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
  show Float = "float"
  show Double = "double"
  show Ast.CAst.Void = "void"
  show Ast.CAst.Bool = "bool"
  show Ast.CAst.Char = "char"
  show (Ptr t) = show t ++ "*"

toCType :: Type -> CType
toCType (Int I8 Signed) = Int8
toCType (Int I8 Unsigned) = UInt8
toCType (Int I16 Signed) = Int16
toCType (Int I16 Unsigned) = UInt16
toCType (Int I32 Signed) = Int32
toCType (Int I32 Unsigned) = UInt32
toCType (Int I64 Signed) = Int64
toCType (Int I64 Unsigned) = UInt64
toCType Lib.Types.Void = Ast.CAst.Void
toCType Lib.Types.Bool = Ast.CAst.Bool
toCType Lib.Types.Char = Ast.CAst.Char
toCType (Arr t) = Ptr (toCType t)
toCType Str = Ptr Ast.CAst.Char

data Function
  = Func CType QualifiedName [(CType, Name)] [Statement] Expr
  | Proc QualifiedName [(CType, Name)] [Statement]
  deriving (Eq, Ord)

formatParams :: [(CType, Name)] -> String
formatParams params = tail $ concatMap (\(tpe,name) -> show tpe ++ " " ++ show name ++ ",") params

instance Show Function where
  show (Func tpe name params stmnts ret) =
    show tpe ++ " " ++ show name ++ "( " ++ formatParams params ++ ")" ++ "{"  ++ concatMap (\x -> show x ++ "\n") stmnts ++ "return " ++ show ret ++ ";\n}"
  show (Proc name params stmnts) =
    "void " ++ show name ++ "( " ++ formatParams params ++ ")" ++ "{"  ++ concatMap (\x -> show x ++ "\n") stmnts ++ "}"

data Statement
  = SExpr Expr
  | SDecl Name CType
  | SDeclArr Name CType [Expr]
  | SDeclAssign Name CType Expr
  | SBlock [Statement]
  | SWhile Expr [Statement]
  | SIf Expr [Statement]
  deriving (Eq, Ord)
instance Show Statement where
  show (SExpr e) = show e ++ ";\n"
  show (SBlock s) = "{\n" ++ show s ++ "\n}"
  show (SDecl name tpe) = show tpe ++ " " ++ show name ++ ";\n"
  show (SDeclAssign name tpe expr) = show tpe ++ " " ++ show name ++ " = " ++ show expr ++ ";\n"
  show (SDeclArr name tpe expr) = show tpe ++ " " ++ show name ++ " = " ++ show expr ++ ";\n"
  show (SWhile e stmnt) = "while (" ++ show e ++ ")\n" ++ show stmnt
  show (SIf e stmnt) = "if (" ++ show e ++ ")\n" ++ show stmnt

data Expr
 = BOp BinOp Expr Expr
 | EAssign Name Expr
 | EAssignArr Expr Expr Expr
 | UOp UnOp Expr
 | Lit Int
 | Var Name
 | FuncName QualifiedName
 | Ch Char
 | Call QualifiedName [Expr]
  deriving (Eq, Ord)
instance Show Expr where
  show (BOp op e1 e2) = show e1 ++ " " ++ show op ++ " " ++ show e2
  show (EAssign name expr) = show name ++ " = " ++ show expr
  show (EAssignArr e1 e2 e3) = show e1 ++ "[" ++ show e2 ++ "] = " ++ show e3
  show (UOp op e1) = show op ++ " " ++ show e1
  show (Lit i) = show i
  show (Var name) = show name
  show (FuncName name) = show name
  show (Ch char) = show char
  show (Call name exprs) = show name ++ "(" ++ show exprs ++ ")"

