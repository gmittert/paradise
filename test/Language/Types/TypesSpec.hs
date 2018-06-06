module Language.Types.TypesSpec where

import Test.Hspec
import Testing

spec :: Spec
spec = do
  describe "Type constructors" $ do
    it "should make unions" $ do
      exitOf "samples/types/union.para" `shouldReturn` Right 3
    it "should make records" $ do
      exitOf "samples/types/rec.para" `shouldReturn` Right 3
  describe "Pattern matching" $ do
    it "should work on records"$
      exitOf "samples/types/case_rec.para" `shouldReturn` Right 3
    it "should work on unions"$
      exitOf "samples/types/case_union.para" `shouldReturn` Right 3

main :: IO ()
main = hspec spec
