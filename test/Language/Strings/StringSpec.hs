module Language.Strings.StringSpec where

import Test.Hspec
import Testing

spec :: Spec
spec = do
  describe "Strings" $
    it "samples/strings/str.para should print strings" $
    stdoutOf "samples/strings/str.para" `shouldReturn` Right "Hello"

main :: IO ()
main = hspec spec
