module TyperSpec where
import Test.Hspec
import Compile

isError :: Either a b -> Bool
isError (Right _) = False
isError (Left _) = True

isSucc :: Either a b -> Bool
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
         res1 <- compileString progFail
         res1 `shouldSatisfy` isError
         res2 <- compileString progSucc
         res2 `shouldSatisfy` isSucc

main :: IO()
main = hspec spec
