module TyperSpec where
import Test.Hspec
import Compile

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
                   \i64 add(i64 x, i64 y) {return 0;}\n\
                   \i64 main(){return add(1);}\n"

           progSucc = "\
                   \i64 add(i64 x, i64 y) {1+2; return 0;}\n\
                   \i64 main(){1+2; return add(1, 2);}\n"
       in do
         compileString progFail `shouldSatisfy` isError
         compileString progSucc `shouldSatisfy` isSucc

main :: IO()
main = hspec spec
