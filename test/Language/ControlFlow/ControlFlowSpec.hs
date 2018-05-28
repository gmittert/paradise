module Language.ControlFlow.ControlFlowSpec where

import Test.Hspec
import Testing

spec :: Spec
spec = do
  describe "If statements" $ do
    it "don't execute on false" $
      exitOf "samples/flow/if.para" `shouldReturn` Right 3
    it "execute on true" $
      exitOf "samples/flow/if2.para" `shouldReturn` Right 3
  describe "While statements" $ do
    it "execute until the condition is false" $
      exitOf "samples/flow/while1.para" `shouldReturn` Right 3

main :: IO ()
main = hspec spec
