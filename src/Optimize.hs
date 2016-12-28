mqodule Optimize where

import TAC
import Prelude hiding (succ)
import Types

data FlowGraph
  = FlowGraph {   block :: TacTree
                , succ   :: [FlowGraph]
                , liveVars :: [Name]
              }
  deriving (Eq, Ord, Show)
optimize :: TacTree -> FlowGraph
optimize = optimize' . buildFlowGraph

optimize' :: FlowGraph -> FlowGraph
optimize' = id

buildFlowGraph :: TacTree -> FlowGraph
buildFlowGraph (IAddr a) = FlowGraph (IAddr a) [] []
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
