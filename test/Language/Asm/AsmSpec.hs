module Language.Asm.AsmSpec where

import Test.Hspec
import Testing

spec :: Spec
spec = do
  describe "Asm blocks" $ do
    it "should modify variables"$
      exitOf "samples/asm/asm.para" `shouldReturn` Right 3

main :: IO ()
main = hspec spec
