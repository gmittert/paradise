{- |
Module      : OpenCLAst
Description : Represents an OpenCL kernel
Copyright   : GWen Mittertreiner, 2018
-}
module Ast.OpenCLAst where

import Lib.Types
import Lib.Format

data CLType
  = CLLong -- ^ long
  | CLInt -- ^ int
  | CLVoid -- ^ void
  | CLPtr CLType -- ^ A pointer
  | Global CLType -- ^ A global value
instance Show CLType where
  show CLLong = "long"
  show CLInt = "int"
  show CLVoid = "void"
  show (CLPtr t) = show t ++ "*"
  show (Global t) = "__global " ++ show t

data CLParam = CLParam Type Name
instance Show CLParam where
  show (CLParam t n) = show t ++ " " ++ show n

data Kernel = Kernel {
  name :: Name -- ^ The kernel name
  , tpe :: CLType
  , params :: [CLParam] -- ^ The params passed to the kernel
  , stms :: [Stm]
  , outputs :: [Name] -- ^ The list of names the kernel modifies
}
instance Show Kernel where
  show (Kernel name tpe params stms _) = concat
      ["__kernel ", show tpe, " ", show name, "( ", commaList params, ") {\n"
      , semiList stms
      , "}\n"]
data Exp
  = BinOp CLOp Exp Exp
  | Var Name
instance Show Exp where
  show (BinOp op e1 e2) = concat [show e1, " ", show op, " ", show e2]
  show (Var n) = concat [show n, " [i]"]
data Stm
  = CLAssign Name Exp -- arr1[i] = arr2[i]
  | GetGlobalId -- const int i
instance Show Stm where
  show (CLAssign n1 e) = concat [
    show n1, "[i] = ", show e]
  show GetGlobalId = "const int i = get_global_id(0)"

data CLOp
  = CLMult
  | CLPlus
instance Show CLOp where
  show CLMult = "*"
  show CLPlus = "+"
