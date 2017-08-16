module Grapher where
import Lib.IR
import qualified Lib.Graph as G

grapher :: [(IRInstr, Int)] -> Either String (G.Graph [IRInstr])
grapher blocks = undefined
