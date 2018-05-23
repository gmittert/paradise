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

weeder :: M.Map ModulePath PA.Module-> Either CompileError (M.Map ModulePath WA.Module)
weeder mods = case forM mods weedProg of
  Left error -> Left $ WeederE error
  Right a -> Right a

weedProg :: PA.Module -> Either WeederError WA.Module
weedProg (PA.Module name imprts cfuncs funcs _) = WA.Module (fileToModulePath name) imprts cfuncs <$> forM funcs weedFunc

weedFunc :: PA.Function -> Either WeederError WA.Function
weedFunc (PA.Func tpe name args stmnts exp _) = do
  let duplicateDefs = any (\x -> length x > 1) . group . sort . map snd
  if duplicateDefs args
    then Left (WeederError ("Duplicate argument definitions in " ++ show name) [] [])
    else WA.Func tpe name args <$> forM stmnts weedStmnt <*> weedExpr exp
weedFunc (PA.Proc name args stmnts _) = do
  let duplicateDefs = any (\x -> length x > 1) . group . sort . map snd
  if duplicateDefs args
    then Left $ WeederError ("Duplicate argument definitions in " ++ show name) [] []
    else WA.Func Void name args <$> forM stmnts weedStmnt <*> return WA.Unit

weedStmnt :: PA.Statement -> Either WeederError WA.Statement
weedStmnt (PA.SExpr expr _) = WA.SExpr <$> weedExpr expr
weedStmnt (PA.SDecl name tpe _) = return $ WA.SDecl name tpe
weedStmnt (PA.SDeclAssign name tpe expr _) = WA.SDeclAssign name tpe <$> weedExpr expr
weedStmnt (PA.SBlock stmnts _) = WA.SBlock <$> forM stmnts weedStmnt
weedStmnt (PA.SWhile expr stmnt _) = WA.SWhile <$> weedExpr expr <*> weedStmnt stmnt
weedStmnt (PA.SIf expr stmnt _) = WA.SIf <$> weedExpr expr <*> weedStmnt stmnt
weedStmnt (PA.ForEach name expr stmnt _) = WA.ForEach name <$> weedExpr expr <*> weedStmnt stmnt
weedStmnt (PA.Kernel k _) = WA.Kernel <$> weedKExpr k

weedExpr :: PA.Expr -> Either WeederError WA.Expr
weedExpr (PA.BOp op exp1 exp2 _) = WA.BOp op <$> weedExpr exp1 <*> weedExpr exp2
weedExpr (PA.UOp op e _) = WA.UOp op <$> weedExpr e
weedExpr (PA.Lit n sz s _) = checkLitBounds (WA.Lit n sz s) >>= specNeg
weedExpr (PA.FLit l sz _) = return $ WA.FLit l sz
weedExpr (PA.Var v _) = return $ WA.Var v
weedExpr (PA.Ch c _) = return $ WA.Ch c
weedExpr (PA.Call name exprs _) = WA.Call name <$> forM exprs weedExpr
weedExpr (PA.Str s _) = return $ WA.ArrLit (WA.Ch <$> s)
weedExpr (PA.ArrLit e _) = WA.ArrLit <$> forM e weedExpr
weedExpr (PA.ListComp e _) = WA.ListComp <$> weedListComp e

weedKExpr :: PA.KExpr -> Either WeederError WA.KExpr
weedKExpr (PA.KBOp op ke1 ke2 _) = WA.KBOp op <$> weedKExpr ke1 <*> weedKExpr ke2
weedKExpr (PA.KName n _) = return $ WA.KName n

weedListComp :: PA.ListExpr -> Either WeederError WA.ListExpr
weedListComp (PA.LFor e var le _) = WA.LFor <$> weedExpr e <*> return var <*> weedExpr le
weedListComp (PA.LRange lst to _) = do
  from' <- forM lst weedExpr
  to' <- weedExpr to
  case from' of
    [a, b] -> return $ WA.LRange a b to'
    [a] -> return $ WA.LRange a (WA.BOp Plus (WA.Lit 1 IUnspec SUnspec) a) to'
    _ -> Left $ WeederError "Invalid range expression: " [] lst

-- | Check that literals are within the bounds for the type
checkLitBounds :: WA.Expr -> Either WeederError WA.Expr
checkLitBounds l@(WA.Lit n sz s) = let
  (lower, upper) = (case s of
    Unsigned -> case sz of
      I1 -> (0, 1)
      I8 -> (0, 2^8 - 1)
      I16 -> (0, 2^16 - 1)
      I32 -> (0, 2^32 - 1)
      I64 -> (0, 2^64 - 1)
      IUnspec -> (0, 2^64 - 1)
    Signed -> case sz of
      I1 -> (-(2^1), 0)
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
