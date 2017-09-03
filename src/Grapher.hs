module Grapher where
import qualified Lib.IR as IR
import qualified Lib.Graph as G

grapher :: [(IR.Stm, Int)] -> Either String (G.Graph [IR.Stm])
grapher blocks = undefined
