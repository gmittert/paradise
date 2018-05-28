{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module GenCL where

import qualified Ast.TypedAst as TA
import qualified Ast.OpenCLAst as CLA
import Control.Monad.State.Lazy
import Lib.Types

data CLState = CLState
  {
    kernelNum :: Int -- Counter to generate unique kernels
  }

emptyCLState :: CLState
emptyCLState = CLState 0

newtype GenCL a = GenCL
  { runCLState :: State CLState a
  } deriving (Functor, Applicative, Monad, MonadState CLState)

mkKName :: GenCL Name
mkKName = do
  i <- gets kernelNum
  let name = "kernel" ++ show i
  modify $ \s -> s{kernelNum = i + 1}
  return (Name name)

genCL :: TA.KExpr -> GenCL CLA.Kernel
genCL a = do
  name <- mkKName
  let tpe = CLA.CLVoid
  params <- getParams a
  stms <- genCLStm a
  outputs <- getOutputs a
  return $ CLA.Kernel name tpe params stms outputs

-- | Figure out which variables need to be passed in to the kernel
getParams :: TA.KExpr -> GenCL [CLA.CLParam]
getParams (TA.KBOp KAssign _ e2 _) = getParams e2
getParams (TA.KBOp _ e1 e2 _) = liftM2 (++) (getParams e1) (getParams e2)
getParams (TA.KName n _ t) = return [CLA.CLParam t n]

getOutputs :: TA.KExpr -> GenCL [Name]
getOutputs (TA.KBOp KAssign e1 _ _) = getOutputs e1
getOutputs (TA.KBOp _ _ _ _) = return []
getOutputs (TA.KName n _ _) = return [n]

genCLStm :: TA.KExpr -> GenCL [CLA.Stm]
genCLStm (TA.KBOp KAssign (TA.KName n _ _) e _) = do
    e' <- genCLExpr e
    return $ [CLA.CLAssign n e']
genCLStm TA.KName{} = error "Unexpected top level name"
genCLStm a = error $ "Unexpected top level expression: " ++ show a

genCLExpr :: TA.KExpr -> GenCL CLA.Exp
genCLExpr (TA.KBOp op e1 e2 _) = do
  e1' <- genCLExpr e1
  e2' <- genCLExpr e2
  return $ CLA.BinOp (kopToCLOp op) e1' e2'
genCLExpr (TA.KName n _ _) = return $ CLA.Var n

kopToCLOp :: KBinOp -> CLA.CLOp
kopToCLOp ElemMult = CLA.CLMult
kopToCLOp ElemPlus = CLA.CLPlus
kopToCLOp a = error $ "Operator " ++ show a ++ " not supported yet"
