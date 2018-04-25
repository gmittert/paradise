{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Weeder where
import qualified Ast.ParsedAst as PA
import qualified Ast.WeededAst as WA
import Control.Monad
import Data.List
import qualified Data.Map.Strict as M
import Lib.Types
import Errors.WeederError
import Errors.CompileError

weeder :: M.Map ModulePath PA.Module-> Either String (M.Map ModulePath WA.Module)
weeder mods = case forM mods weedProg of
  Left error -> Left $ Errors.CompileError.toString error
  Right a -> Right a

weedProg :: PA.Module -> Either WeederError WA.Module
weedProg (PA.Module name  imprts funcs) = WA.Module (fileToModulePath name) imprts <$> forM funcs weedFunc

weedFunc :: PA.Function -> Either WeederError WA.Function
weedFunc (PA.Func tpe name args stmnts exp) = do
  let duplicateDefs = any (\x -> length x > 1) . group . sort . map snd
  if duplicateDefs args
    then Left (WeederError ("Duplicate argument definitions in " ++ show name) [] [])
    else WA.Func tpe name args <$> forM stmnts weedStmnt <*> weedExpr exp
weedFunc (PA.Proc name args stmnts) = do
  let duplicateDefs = any (\x -> length x > 1) . group . sort . map snd
  if duplicateDefs args
    then Left $ WeederError ("Duplicate argument definitions in " ++ show name) [] []
    else WA.Proc name args <$> forM stmnts weedStmnt
weedFunc (PA.CFunc tpe name args body) = do
  let duplicateDefs = any (\x -> length x > 1) . group . sort . map snd
  if duplicateDefs args
    then Left $ WeederError ("Duplicate argument definitions in " ++ show name) [] []
    else return $ WA.CFunc tpe name args body

weedStmnt :: PA.Statement -> Either WeederError WA.Statement
weedStmnt (PA.SExpr expr) = WA.SExpr <$> weedExpr expr
weedStmnt (PA.SDecl name tpe) = return $ WA.SDecl name tpe
weedStmnt (PA.SDeclArr name tpe exprs) = WA.SDeclArr name tpe <$> forM exprs weedExpr
weedStmnt (PA.SDeclAssign name tpe expr) = WA.SDeclAssign name tpe <$> weedExpr expr
weedStmnt (PA.SBlock stmnts) = WA.SBlock <$> forM stmnts weedStmnt
weedStmnt (PA.SWhile expr stmnt) = WA.SWhile <$> weedExpr expr <*> weedStmnt stmnt
weedStmnt (PA.SIf expr stmnt) = WA.SIf <$> weedExpr expr <*> weedStmnt stmnt
weedStmnt (PA.ForEach name expr stmnt) = WA.ForEach name <$> weedExpr expr <*> weedStmnt stmnt
weedStmnt (PA.Kernel k) = WA.Kernel <$> weedKExpr k

weedExpr :: PA.Expr -> Either WeederError WA.Expr
weedExpr (PA.BOp op exp1 exp2) = WA.BOp op <$> weedExpr exp1 <*> weedExpr exp2
weedExpr (PA.EAssign name e) = WA.EAssign name <$> weedExpr e
weedExpr (PA.UOp op e) = WA.UOp op <$> weedExpr e
weedExpr (PA.Lit n sz s) = checkLitBounds (WA.Lit n sz s) >>= specNeg
weedExpr (PA.FLit l sz) = return $ WA.FLit l sz
weedExpr (PA.Var v) = return $ WA.Var v
weedExpr (PA.Ch c) = return $ WA.Ch c
weedExpr (PA.EAssignArr e1 e2 e3) = WA.EAssignArr <$> weedExpr e1 <*> weedExpr e2 <*> weedExpr e3
weedExpr (PA.Call name exprs) = WA.Call name <$> forM exprs weedExpr

weedKExpr :: PA.KExpr -> Either WeederError WA.KExpr
weedKExpr (PA.KBOp op ke1 ke2) = WA.KBOp op <$> weedKExpr ke1 <*> weedKExpr ke2
weedKExpr (PA.KName n) = return $ WA.KName n

-- | Check that literals are within the bounds for the type
checkLitBounds :: WA.Expr -> Either WeederError WA.Expr
checkLitBounds l@(WA.Lit n sz s) = let
  (lower, upper) = (case s of
    Unsigned -> case sz of
      I8 -> (0, 2^8 - 1)
      I16 -> (0, 2^16 - 1)
      I32 -> (0, 2^32 - 1)
      I64 -> (0, 2^64 - 1)
      IUnspec -> (0, 2^64 - 1)
    Signed -> case sz of
      I8 -> (-(2^7), 2^7 - 1)
      I16 -> (-(2^15), 2^15 - 1)
      I32 -> (-(2^31), 2^31 - 1)
      I64 -> (-(2^63), 2^63 - 1)
      IUnspec -> (-(2^63), 2^63 - 1)
    SUnspec -> (-(2^63), 2^64 - 1)) :: (Integer, Integer) in
  if fromIntegral n >= lower && fromIntegral n <= upper then return l
  else Left (WeederError ("Value " ++ show n ++ " is out of range for " ++ show sz ++ " range: " ++ show (lower, upper)) [] [])
checkLitBounds a = return a

-- | If a literal is negative and doesn't have a specified sign, we know we can
-- fill it in as Signed
specNeg :: WA.Expr -> Either WeederError WA.Expr
specNeg l@(WA.Lit n sz SUnspec) = if n < 0 then return (WA.Lit n sz Signed) else return l
specNeg n = return n
