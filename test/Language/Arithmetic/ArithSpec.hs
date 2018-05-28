module Language.Arithmetic.ArithSpec where

import Test.Hspec
import Testing

spec :: Spec
spec = do
  describe "Adding" $ do
    it "should add numbers" $
      exitOf "samples/basic/add.para" `shouldReturn` Right 3
  describe "Subtracting" $ do
    it "should subtract numbers" $
      exitOf "samples/basic/sub.para" `shouldReturn` Right 3
  describe "Division" $ do
    it "should divide numbers" $
      exitOf "samples/basic/div.para" `shouldReturn` Right 3
  describe "Multiplication" $ do
    it "should multiply numbers" $
      exitOf "samples/basic/mul.para" `shouldReturn` Right 3

main :: IO ()
main = hspec spec
