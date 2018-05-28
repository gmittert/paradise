module Language.Functions.FunctionSpec where

import Test.Hspec
import Testing

spec :: Spec
spec = do
  describe "Functions" $ do
    it "samples/func/func.para: should compile functions with no args" $
      exitOf "samples/func/func.para" `shouldReturn` Right 3
    it "samples/func/func2.para: should compile functions with args" $
      exitOf "samples/func/func2.para" `shouldReturn` Right 3
    it "samples/func/recursion.para: should compile functions with recursion" $
      exitOf "samples/func/recursion.para" `shouldReturn` Right 120
main :: IO ()
main = hspec spec
