module WeederSpec where
import Test.Hspec
import Compile

isError :: Either a b -> Bool
isError (Right _) = False
isError (Left _) = True

spec :: Spec
spec = do
  describe "Functions" $ do
    it "should not have duplicate arguments" $
       let prog = "\
                   \int add(int x, int x) {return 0;}\n\
                   \int main(){return add(1,2);}\n"
       in do
         res <- compileString prog
         res `shouldSatisfy` isError

main :: IO()
main = hspec spec
