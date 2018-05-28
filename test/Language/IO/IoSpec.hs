module Language.IO.IoSpec where

import Test.Hspec
import Testing

spec :: Spec
spec = do
  describe "IO" $
    it "samples/io/readFile.para should read files" $
    stdoutOf "samples/io/readFile.para" `shouldReturn` Right "Hello World"
main :: IO ()
main = hspec spec
