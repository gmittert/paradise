module Grapher where
import qualified Lib.IR as IR
import qualified Lib.Graph as G

grapher :: [(IR.Instr, Int)] -> Either String (G.Graph [IR.Instr])
grapher blocks = undefined
