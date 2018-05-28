module Language.Modules.ModuleSpec where

import Test.Hspec
import Testing

spec :: Spec
spec = do
  describe "Including" $
    it "samples/include/includer.para should include files" $
    exitOf "samples/include/includer.para" `shouldReturn` Right 0
main :: IO ()
main = hspec spec
