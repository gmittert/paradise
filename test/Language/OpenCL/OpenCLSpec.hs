module Language.OpenCL.OpenCLSpec where

import Test.Hspec
import Testing

spec :: Spec
spec = do
  describe "OpenCL" $ do
    it "samples/opencl/kernel1.para should square numbers" $
      exitOf "samples/opencl/kernel1.para" `shouldReturn` Right 3
    it "samples/opencl/kernel1.para should square numbers on its own array" $
      exitOf "samples/opencl/kernel2.para" `shouldReturn` Right 3
    it "samples/opencl/kernel1.para should work with floats" $
      exitOf "samples/opencl/kernel3.para" `shouldReturn` Right 3

main :: IO ()
main = hspec spec
