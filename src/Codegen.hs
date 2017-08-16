module Codegen where

import Lib.IR
import qualified Lib.Graph as G
import Asm

codegen :: G.Graph [IRInstr] -> Either String [AInstr]
codegen = undefined
