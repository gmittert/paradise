module Ast.CAst where

import Lib.Types
import Lib.Format
import GenCState
import qualified Data.Set as S
import Control.Monad.State.Lazy

includes :: String
includes = unlines [
  "#include <stdio.h>"
  , "#include <stdint.h>"
  , "#include <stdlib.h>"
  , "#include <stddef.h>"
  , "#include <CL/cl.h>"
  , "void checkError(cl_int error) {"
  , "  if (error != CL_SUCCESS) {"
  , "    printf(\"OpenCL call failed with error %d\\n\", error);"
  , "    exit(1);"
  , "  }"
  , "}"
  ,"char* readFile(const char* fname, size_t* size) {"
  ,"  FILE * f = fopen (fname, \"rb\");"
  ,"  fseek (f, 0, SEEK_END);"
  ,"  *size = ftell(f);"
  ,"  fseek (f, 0, SEEK_SET);"
  ,"  char* buffer = malloc (*size);"
  ,"  if (buffer) {"
  ,"    fread (buffer, 1, *size, f);"
  ,"  } else {"
  ,"    fprintf(stderr, \"Failed to allocate a buffer of the kernel program\\n\");"
  ,"    exit(1);"
  ,"  }"
  ,"  fclose (f);"
  ,"  return buffer;"
  ,"}"
  ]

clSetup :: String
clSetup = unlines [
  "  cl_uint numPlatforms = 0;"
  ,"  clGetPlatformIDs(0, NULL, &numPlatforms);"
  ,"  if (numPlatforms == 0) {"
  ,"    printf(\"No OpenCL platforms found\\n\");"
  ,"    return 1;"
  ,"  }"
  ,"  cl_uint platformIdCount = 0;"
  ,"  clGetPlatformIDs(0, NULL, &platformIdCount);"
  ,""
  ,"  cl_platform_id *platformIds = malloc(numPlatforms);"
  ,"  clGetPlatformIDs(platformIdCount, platformIds, NULL);"
  ,""
  ,"  cl_uint deviceIdCount = 0;"
  ,"  clGetDeviceIDs(platformIds[0], CL_DEVICE_TYPE_ALL, 0, NULL, &deviceIdCount);"
  ,""
  ,"  if (deviceIdCount == 0) {"
  ,"    printf(\"No OpenCL devices found\\n\");"
  ,"    return 1;"
  ,"  }"
  ,""
  ,"  cl_device_id *deviceIds = malloc(deviceIdCount);"
  ,"  clGetDeviceIDs(platformIds[0], CL_DEVICE_TYPE_ALL, deviceIdCount, deviceIds,"
  ,"                 NULL);"
  ,""
  ,"  const cl_context_properties contextProperties[] = {"
  ,"      CL_CONTEXT_PLATFORM, (cl_context_properties)(platformIds[0]), 0};"
  ,""
  ,"  cl_int error = CL_SUCCESS;"
  ,"  cl_context context = clCreateContext(contextProperties, deviceIdCount,"
  ,"                                       deviceIds, NULL, NULL, &error);"
  ,"  checkError(error);"
  , "cl_command_queue queue = "
  , "   clCreateCommandQueueWithProperties(context, deviceIds[0], 0, &error);"
  , " checkError(error);"
  ]

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
  show Int32 = "int"
  show UInt32 = "unsigned int"
  show Int64 = "long"
  show UInt64 = "unsigned long"
  show Ast.CAst.Int = "int"
  show Ast.CAst.Float = "float"
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
toCType (Lib.Types.Int _ _) = error "All ints should be fully specified by this point"
toCType (Lib.Types.Float F32) = return Ast.CAst.Float
toCType (Lib.Types.Float F64) = return Ast.CAst.Double
toCType (Lib.Types.Float _) = error "All float should be fully specified by this point"
toCType Lib.Types.Void = return Ast.CAst.Void
toCType Lib.Types.Bool = return Ast.CAst.Bool
toCType Lib.Types.Char = return Ast.CAst.Char
toCType (Arr t) = do
  tpe' <- toCType t
  modify $ \s -> s{defs = S.insert (makeArrDec tpe') (defs s)}
  return $ makeArr tpe'
toCType Str = return $ Ptr Ast.CAst.Char
toCType TUnspec = error "All types should be specified by this point"
toCType (F _ _) = error "Function types not supported yet"

makeArrDec :: CType -> Statement
makeArrDec t = StructDef (map (\x -> if x == ' ' then '_' else x) (show t) ++ "_arr") [(Ptr t, "data"), (UInt32, "len")]

makeArr :: CType -> CType
makeArr t = Struct (map (\x -> if x == ' ' then '_' else x) (show t) ++ "_arr")

data Function
  = Func CType QualifiedName [(CType, Name)] [Statement] Expr
  | CFunc String
  deriving (Eq, Ord)

formatParams :: [(CType, Name)] -> String
formatParams params = tail $ concatMap (\(tpe,name) -> ", " ++ show tpe ++ " " ++ show name) params ++ " "

instance Show Function where
  show (Func tpe name params stmnts ret) = case name of
    (QualifiedName (ModulePath []) (Name "main")) -> show tpe ++ " " ++ show name ++ "( " ++ formatParams params ++ ")" ++ "{\n" ++ clSetup ++ concatMap show stmnts ++ (if ret == Unit then [] else "return " ++ show ret ++ ";") ++ "\n}"
    _ -> show tpe ++ " " ++ show name ++ "( " ++ formatParams params ++ ")" ++ "{\n"  ++ concatMap show stmnts ++ "return " ++ show ret ++ ";\n}"
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
  | OpenCLStm [CLStm]
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
    (CFunc _) -> ""
  show (OpenCLStm stms) = concatMap show stms

data Expr
 = BOp BinOp Expr Expr
 | EAssign Name Expr
 | EAssignArr Expr Expr Expr
 | Field Expr String
 | EAssignF Expr String Expr
 | UOp UnOp Expr
 | Lit Int
 | FLit Double
 | CStr String
 | Var Name
 | FuncName QualifiedName
 | Ch Char
 | Unit
 | Call QualifiedName [Expr]
 | Malloc Expr
 | Free String
 | CompoundLit String [Expr]
  deriving (Eq, Ord)
instance Show Expr where
  show (BOp Access e1 e2) = show e1 ++ ".data[" ++ show e2 ++ "]"
  show (BOp op e1 e2) = show e1 ++ " " ++ show op ++ " " ++ show e2
  show (EAssign name expr) = show name ++ " = " ++ show expr
  show (EAssignArr e1 e2 e3) = show e1 ++ "[" ++ show e2 ++ "] = " ++ show e3
  show (UOp Len e1) = show e1 ++ ".len"
  show (UOp op e1) = show op ++ " " ++ show e1
  show (Lit i) = show i
  show (FLit i) = show i
  show (CStr s) = "\"" ++ s ++ "\""
  show (Var name) = show name
  show (FuncName name) = show name
  show (Ch char) = show char
  show Unit = error "Unit should not need to be shown"
  show (Field exp f) = show exp ++ "." ++ f
  show (EAssignF exp1 f exp2) = show exp1 ++ "." ++ f ++ " = " ++ show exp2
  show (Call name exprs) = show name ++ "(" ++ commaList exprs++ ")"
  show (Malloc expr) = "malloc(" ++ show expr ++ ")"
  show (Free name) = "free(" ++ name ++ ")"
  show (CompoundLit n exprs) = "((struct " ++ n ++ ") {" ++ commaList exprs ++ "})"

data Kernel = Kernel Type Name [(Type, Name)] KExpr
  deriving (Eq, Ord)
instance Show Kernel where
  show (Kernel tpe n args e) = "__kernel " ++ show tpe ++ " " ++ show n ++
   commaList (map (\(tpe,name) -> "__global " ++ show tpe ++ " " ++ show name) args)
   ++ ") { const int i get_global_id(0);" ++ show e ++ "}"

data KExpr
  = KName Name
  | KAccess KExpr
  | KOp BinOp KExpr KExpr
  deriving (Eq, Ord)
instance Show KExpr where
  show (KName n) = show n
  show (KAccess e) = show e ++ "[i]"
  show (KOp op e1 e2) = show e1 ++ " " ++ show op ++ " " ++ show e2

data CLStm
  = CreateBuffer {var::Name, tpe :: CType}
  | SetKernelArg Int Name
  | EnqueueNDRangeKernel {var :: Name}
  | ReadBuff {var :: Name, tpe :: CType}
  | ReleaseBuff Name
  | BuildProgram {source :: String, tpe :: CType, args :: [(CType, Name)]}
  deriving (Eq, Ord)
instance Show CLStm where
  show (CreateBuffer var tpe) = "cl_mem " ++ show var ++ "_buff = clCreateBuffer(context, CL_MEM_READ_WRITE | CL_MEM_COPY_HOST_PTR, sizeof(" ++ show tpe ++ ") * " ++ show var ++ ".len, " ++ show var ++ ".data, &error); checkError(error);"
  show (SetKernelArg i name) = "clSetKernelArg(kernel, " ++ show i ++ ", sizeof(cl_mem), &"++ show name ++ "_buff);"
  show (EnqueueNDRangeKernel var) = "size_t global_size[1] = {" ++ show var++ ".len}; checkError(clEnqueueNDRangeKernel(queue, kernel, 1, NULL, global_size, NULL, 0, NULL, NULL));"
  show (ReadBuff var tpe) = "checkError(clEnqueueReadBuffer(queue, "++ show var ++ "_buff, CL_TRUE, 0, sizeof(" ++ show tpe ++ ") * "++ show var ++ ".len, " ++ show var ++ ".data, 0, NULL, NULL));"
  show (ReleaseBuff var) = "clReleaseMemObject("++ show var ++ "_buff);"

  show (BuildProgram source tpe args) =
    let kernel = "__kernel " ++ show tpe ++ " MYPROG (" ++ commaListS (map (\(x,y) -> "__global " ++ show x ++ "* " ++ show y) args) ++ ") { const int i = get_global_id(0); " ++ source ++ ";}" in
      "const char* source = \"" ++ kernel ++ "\";\nsize_t prog_size = " ++ show (length kernel)++ ";\ncl_program program = clCreateProgramWithSource(context, 1, &source, &prog_size, &error); checkError(error);\n checkError(clBuildProgram(program, deviceIdCount, deviceIds, NULL, NULL, NULL));\n cl_kernel kernel = clCreateKernel(program, \"MYPROG\", &error); checkError(error);"
