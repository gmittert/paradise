module TyperSpec where
import Test.Hspec
import Compile
import Control.Monad

isError :: Either String String -> Bool
isError (Right _) = False
isError (Left _) = True

isSucc :: Either String String -> Bool
isSucc = not . isError

spec :: Spec
spec = do
  describe "Functions" $ do
    it "should be passed the correct number of arguments" $
       let progFail = "\
                   \int add(int x, int y) {return 0;}\n\
                   \int main(){return add(1);}\n"

           progSucc = "\
                   \int add(int x, int y) {return 0;}\n\
                   \int main(){return add(1, 2);}\n"
       in do
         process progFail `shouldSatisfy` isError
         process progSucc `shouldSatisfy` isSucc

main :: IO()
main = hspec spec
