module Language.Asm.AsmSpec where

import Test.Hspec
import Testing

spec :: Spec
spec = do
  describe "Asm blocks" $ do
    it "should modify variables"$
      exitOf "samples/asm/write.para" `shouldReturn` Right 3
    it "should read existing variables"$
      exitOf "samples/asm/read.para" `shouldReturn` Right 3
    it "should make syscalls"$
      stdoutOf "samples/asm/syscall.para" `shouldReturn` Right "Hello!"

main :: IO ()
main = hspec spec
