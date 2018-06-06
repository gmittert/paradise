module FormatSpec where
import Test.Hspec
import Lib.Format
import Test.QuickCheck

spec :: Spec
spec = do
  describe "Comma separated lists" $ do
    it "should be able to be empty" $
         commaList ([]::[Int]) `shouldBe` ""
    it "should be able to be one element" $
         commaList [1] `shouldBe` "1"
    it "should make a list for multiple elements" $
         commaList [1,2,3] `shouldBe` "1,2,3"
    it "should be equivalent to show without brackets" $ property $
      \x -> ("[" ++ commaList x ++ "]") == (show (x :: [Int]))

main :: IO()
main = hspec spec
