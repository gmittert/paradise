module Optimize where

import TAC
import Semantic
import Prelude hiding (succ)
import Types
import qualified Data.Map.Strict as M

data FlowGraph
  = FlowGraph {   block :: TacTree
                , succ   :: [FlowGraph]
                , liveVars :: [Name]
              }
  deriving (Eq, Ord, Show)
optimize :: (TacTree, CodegenState) -> (FlowGraph, CodegenState)
optimize (tree, state) = (optimize' $ (buildFlowGraph tree){liveVars = M.keys ((vars . symTab) state)}, state)

optimize' :: FlowGraph -> FlowGraph
optimize' = id

buildFlowGraph :: TacTree -> FlowGraph
buildFlowGraph (IVal a) = FlowGraph (IVal a) [] []
buildFlowGraph (IName a) = FlowGraph (IName a) [] []
buildFlowGraph (IStr a _) = FlowGraph (IName a) [] []
buildFlowGraph (UInstr op child) =
  let cGraph = buildFlowGraph child in
    FlowGraph (UInstr op (block cGraph)) (succ cGraph) []
buildFlowGraph (Concat first next) =
  let cGraph = buildFlowGraph next in
    FlowGraph (Concat first (block cGraph)) (succ cGraph) []
buildFlowGraph (BInstr op l r) =
  let cGraph = buildFlowGraph r in
    FlowGraph (BInstr op l (block cGraph)) (succ cGraph) []
buildFlowGraph (BAssign a next) =
  let cGraph = buildFlowGraph next in
    FlowGraph (BAssign a (block cGraph)) (succ cGraph) []
buildFlowGraph (Call size next) =
  let cGraph = buildFlowGraph next in
    FlowGraph (Call size (block cGraph)) (succ cGraph) []
