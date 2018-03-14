module Ast.AddressedAst where
import Lib.Types
import Control.Monad.State.Lazy
import qualified Data.Map.Strict as M

newtype Prog = Prog [Function]
  deriving(Eq, Ord)
instance Show Prog where
  show (Prog f) = join $ show <$> f

data Function = Func { ret :: Type
                     , name :: QualifiedName
                     , args :: [(Type, Name)]
                     , locals :: M.Map Name Address
                     , nextOffset :: Int
                     , body :: Statements
                     }

              | AsmFunc { ret :: Type
                         , name :: QualifiedName
                         , args :: [(Type, Name)]
                         , asmbdy :: String
                         }
  deriving(Eq, Ord)
instance Show Function where
  show (Func tpe name tps _ _ stmnt) = show tpe ++ " " ++ show name ++ show tps ++ show stmnt
  show (AsmFunc tpe name tps _) = "asm " ++ show tpe ++ " " ++ show name ++ show tps

data Statements
 = Statements' Statement Type
 | Statements Statements Statement Type
  deriving (Eq, Ord)
instance Show Statements where
  show (Statements' stmnt _) = show stmnt
  show (Statements stmnts stmnt _) = show stmnts ++ show stmnt

data Statement
  = SExpr Expr Type
  | SDecl Name Type Type Address
  | SDeclArr Name Type [Expr] Type Address
  | SDeclAssign Name Type Expr Type Address
  | SBlock Statements Type
  | SWhile Expr Statement Type
  | SIf Expr Statement Type
  | SReturn Expr Type
  deriving (Eq, Ord)
instance Show Statement where
  show (SExpr e _) = show e ++ ";\n"
  show (SBlock s _) = "{\n" ++ show s ++ "\n}"
  show (SDecl name tpe _ _) = show tpe ++ " " ++ show name ++ ";\n"
  show (SDeclArr name tpe exprs _ _) = show tpe ++ " " ++ show name ++ "[" ++ show exprs ++ "]" ++ ";\n"
  show (SDeclAssign name tpe expr _ _) = show tpe ++ " " ++ show name ++ " = " ++ show expr ++ ";\n"
  show (SWhile e stmnt _) = "while (" ++ show e ++ ")\n" ++ show stmnt
  show (SIf e stmnt _) = "if (" ++ show e ++ ")\n" ++ show stmnt
  show (SReturn e _) = "return " ++ show e ++ ";\n"

data Expr
 = BOp BinOp Expr Expr Type
 | EAssign Name Expr Type Address
 | EAssignArr { arr::Expr, index::Expr, val::Expr, tpe::Type }
 | UOp UnOp Expr Type
 | Lit Int IntSize SignType
 | Var Name Type Address VarDir
 | FuncName QualifiedName Type
 | Ch Char Type
 | Call QualifiedName Def [(Expr, Address)] Type
  deriving (Eq, Ord)
instance Show Expr where
  show (BOp op e1 e2 _) = show e1 ++ " " ++ show op ++ " " ++ show e2
  show (EAssign name expr _ _) = show name ++ " = " ++ show expr
  show (EAssignArr e1 e2 e3 _) = show e1 ++ "[" ++ show e2 ++ "] = " ++ show e3
  show (UOp op e1 _) = show op ++ " " ++ show e1
  show (Lit i _ _) = show i
  show (Var name _ _ _) = show name
  show (FuncName name _) = show name
  show (Ch char _) = show char
  show (Call name _ exprs _) = show name ++ "(" ++ show exprs ++ ")"

{-
  Extract the type attached to a statement
-}
getStmntType :: Statement -> Type
getStmntType (SExpr _ tpe) = tpe
getStmntType (SDecl _ _ tpe _) = tpe
getStmntType (SDeclArr _ _ _ tpe _) = tpe
getStmntType (SDeclAssign _ _ _ tpe _) = tpe
getStmntType (SBlock _ tpe) = tpe
getStmntType (SWhile _ _ tpe) = tpe
getStmntType (SIf _ _ tpe) = tpe
getStmntType (SReturn _ tpe) = tpe

{-
  Extract the table attached to a statement
-}
getExprType :: Expr -> Type
getExprType (BOp _ _ _ tpe) = tpe
getExprType (UOp _ _ tpe) = tpe
getExprType (EAssign _ _ tpe _) = tpe
getExprType (Lit _ sz s)  = Int sz s
getExprType (Var _ tpe _ _)  = tpe
getExprType (FuncName _ tpe)  = tpe
getExprType (Ch _ tpe)  = tpe
getExprType (EAssignArr _ _ _ tpe) = tpe
getExprType (Call _ _ _ tpe) = tpe
