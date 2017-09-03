module WeederSpec where
import Test.Hspec
import Compile
import Control.Monad

isError :: Either String String -> Bool
isError (Right _) = False
isError (Left _) = True

spec :: Spec
spec = do
  describe "Functions" $ do
    it "should not have duplicate arguments" $
       let prog = "\
                   \int add(int x, int x) {return 0;}\n\
                   \int main(){return add(1,2);}\n"
       in compile prog `shouldSatisfy` isError

main :: IO()
main = hspec spec
