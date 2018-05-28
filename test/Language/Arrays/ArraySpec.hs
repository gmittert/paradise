module Language.Arrays.ArraySpec where

import Test.Hspec
import Testing

spec :: Spec
spec = do
  describe "Arrays" $ do
    it "should support creating, setting, and getting" $
      exitOf "samples/memory/arr.para" `shouldReturn` Right 3
    it "should support foreach loops" $
      exitOf "samples/memory/arr2.para" `shouldReturn` Right 3
    it "should support byte arrays" $
      exitOf "samples/memory/arr3.para" `shouldReturn` Right 3
    it "should support array literals" $
      exitOf "samples/memory/arrLit.para" `shouldReturn` Right 3
    it "should support array function args" $
      exitOf "samples/memory/arrFunc.para" `shouldReturn` Right 3
main :: IO ()
main = hspec spec
