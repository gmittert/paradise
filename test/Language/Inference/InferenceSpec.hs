module Language.Inference.InferenceSpec where

import Test.Hspec
import Testing

spec :: Spec
spec = do
  describe "TypeInference" $ do
    it "should infer numbers" $
      exitOf "samples/inference/add.para" `shouldReturn` Right 3
    it "should catch type errors" $
      isTypeError "samples/inference/err1.para" `shouldReturn` True

main :: IO ()
main = hspec spec
