{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Weeder where

import qualified Ast.ParsedAst as PA
import qualified Ast.WeededAst as WA
import Control.Monad
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import Errors.CompileError
import Errors.WeederError
import Lib.Types

weeder ::
     M.Map ModulePath PA.Module
  -> Either CompileError (M.Map ModulePath WA.Module)
weeder mods =
  case forM mods weedProg of
    Left error -> Left $ WeederE error
    Right a -> Right a

weedProg :: PA.Module -> Either WeederError WA.Module
weedProg (PA.Module name imprts cfuncs decls p) = do
  let (funcs, types) =
        partition
          (\case
             (PA.FuncDecl _) -> True
             _ -> False)
          decls
  funcs' <- mapM (weedFunc . (\(PA.FuncDecl d) -> d)) funcs
  types' <- mapM (weedType . (\(PA.TypeDecl d) -> d)) types
  return $ WA.Module (fileToModulePath name) imprts cfuncs funcs' types' p

weedType :: TypeDec -> Either WeederError TypeDec
weedType (TypeDec n typs) = return $ TypeDec n typs

weedFunc :: PA.Function -> Either WeederError WA.Function
weedFunc (PA.Func tpe name args stmnts exp p) = do
  let duplicateDefs = any (\x -> length x > 1) . group . sort . map snd
  if duplicateDefs args
    then Left
           (WeederError
              ("Duplicate argument definitions in " ++ show name)
              []
              [])
    else WA.Func tpe name args <$> forM stmnts weedStmnt <*> weedExpr exp <*>
         return p
weedFunc (PA.Proc name args stmnts p) = do
  let duplicateDefs = any (\x -> length x > 1) . group . sort . map snd
  if duplicateDefs args
    then Left $
         WeederError ("Duplicate argument definitions in " ++ show name) [] []
    else WA.Func Void name args <$> forM stmnts weedStmnt <*> return (WA.Unit p) <*>
         return p

weedStmnt :: PA.Statement -> Either WeederError WA.Statement
weedStmnt (PA.SExpr expr p) = WA.SExpr <$> weedExpr expr <*> return p
weedStmnt PA.SDecl {..} = return $ WA.SDecl {..}
weedStmnt (PA.SDeclAssign name tpe expr p) =
  WA.SDeclAssign name tpe <$> weedExpr expr <*> return p
weedStmnt (PA.SBlock stmnts p) =
  WA.SBlock <$> forM stmnts weedStmnt <*> return p
weedStmnt (PA.SWhile expr stmnt p) =
  WA.SWhile <$> weedExpr expr <*> weedStmnt stmnt <*> return p
weedStmnt (PA.SIf expr stmnt p) =
  WA.SIf <$> weedExpr expr <*> weedStmnt stmnt <*> return p
weedStmnt (PA.ForEach name expr stmnt p) =
  WA.ForEach name <$> weedExpr expr <*> weedStmnt stmnt <*> return p
weedStmnt (PA.Kernel k p) = WA.Kernel <$> weedKExpr k <*> return p
weedStmnt (PA.Asm s o i c op p) = do
  let o' = fromMaybe [] o
  let i' = fromMaybe [] i
  return $ WA.Asm s o' i' c op p

weedExpr :: PA.Expr -> Either WeederError WA.Expr
weedExpr (PA.BOp op exp1 exp2 p) =
  WA.BOp op <$> weedExpr exp1 <*> weedExpr exp2 <*> return p
weedExpr (PA.UOp op e p) = WA.UOp op <$> weedExpr e <*> return p
weedExpr PA.Lit {..} = checkLitBounds (WA.Lit {..}) >>= specNeg
weedExpr PA.FLit {..} = return $ WA.FLit {..}
weedExpr PA.Var {..} = return $ WA.Var {..}
weedExpr PA.Ch {..} = return $ WA.Ch {..}
weedExpr (PA.Call name exprs p) =
  WA.Call name <$> forM exprs weedExpr <*> return p
weedExpr (PA.Str s p) = return $ WA.ArrLit (flip WA.Ch p <$> s) p
weedExpr (PA.ArrLit e p) = WA.ArrLit <$> forM e weedExpr <*> return p
weedExpr (PA.ListComp e p) = WA.ListComp <$> weedListComp e <*> return p
weedExpr PA.TypeConstr {..} =
  WA.TypeConstr cname <$> mapM weedExpr exprs <*> return posn
weedExpr PA.Case {..} = do
  e1' <- weedExpr e1
  let weedPatExp (p, e) = do
        p' <- weedPattern p
        e' <- weedExpr e
        return (p', e')
  pats' <- mapM weedPatExp patexps
  return $ WA.Case e1' pats' posn

weedPattern :: PA.Pattern -> Either WeederError WA.Pattern
weedPattern PA.PCh {..} = return $ WA.PCh {..}
weedPattern PA.PLit {..} = return $ WA.PLit {..}
weedPattern PA.PFLit {..} = return $ WA.PFLit {..}
weedPattern PA.PVar {..} = return $ WA.PVar {..}
weedPattern PA.PTypeConstr {..} =
  WA.PTypeConstr name <$> mapM weedPattern pats <*> return posn

weedKExpr :: PA.KExpr -> Either WeederError WA.KExpr
weedKExpr (PA.KBOp op ke1 ke2 p) =
  WA.KBOp op <$> weedKExpr ke1 <*> weedKExpr ke2 <*> return p
weedKExpr PA.KName {..} = return $ WA.KName {..}

weedListComp :: PA.ListExpr -> Either WeederError WA.ListExpr
weedListComp (PA.LFor e var le p) =
  WA.LFor <$> weedExpr e <*> return var <*> weedExpr le <*> return p
weedListComp (PA.LRange lst to p) = do
  from' <- forM lst weedExpr
  to' <- weedExpr to
  case from' of
    [a, b] -> return $ WA.LRange a b to' p
    [a] ->
      return $ WA.LRange a (WA.BOp Plus (WA.Lit 1 IUnspec SUnspec p) a p) to' p
    _ -> Left $ WeederError "Invalid range expression: " [] lst

-- | Check that literals are within the bounds for the type
checkLitBounds :: WA.Expr -> Either WeederError WA.Expr
checkLitBounds l@(WA.Lit n sz s _) =
  let (lower, upper) =
        (case s of
           Unsigned ->
             case sz of
               I1 -> (0, 1)
               I8 -> (0, 2 ^ 8 - 1)
               I16 -> (0, 2 ^ 16 - 1)
               I32 -> (0, 2 ^ 32 - 1)
               I64 -> (0, 2 ^ 64 - 1)
               IUnspec -> (0, 2 ^ 64 - 1)
           Signed ->
             case sz of
               I1 -> (-(2 ^ 1), 0)
               I8 -> (-(2 ^ 7), 2 ^ 7 - 1)
               I16 -> (-(2 ^ 15), 2 ^ 15 - 1)
               I32 -> (-(2 ^ 31), 2 ^ 31 - 1)
               I64 -> (-(2 ^ 63), 2 ^ 63 - 1)
               IUnspec -> (-(2 ^ 63), 2 ^ 63 - 1)
           SUnspec -> (-(2 ^ 63), 2 ^ 64 - 1)) :: (Integer, Integer)
   in if fromIntegral n >= lower && fromIntegral n <= upper
        then return l
        else Left
               (WeederError
                  ("Value " ++
                   show n ++
                   " is out of range for " ++
                   show sz ++ " range: " ++ show (lower, upper))
                  []
                  [])
checkLitBounds a = return a

-- | If a literal is negative and doesn't have a specified sign, we know we can
-- fill it in as Signed
specNeg :: WA.Expr -> Either WeederError WA.Expr
specNeg l@WA.Lit {..} =
  if i < 0
    then return (WA.Lit {sign = Signed, ..})
    else return l
specNeg n = return n
