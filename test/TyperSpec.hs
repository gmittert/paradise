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
                   \I64 add(I64 x, I64 y) {return 0;}\n\
                   \I64 main(){return add(1);}\n"

           progSucc = "\
                   \I64 add(I64 x, I64 y) {1+2; return 0;}\n\
                   \I64 main(){1+2; return add(1, 2);}\n"
       in do
         res1 <- compileString progFail
         res1 `shouldSatisfy` isError
         res2 <- compileString progSucc
         res2 `shouldSatisfy` isSucc

main :: IO()
main = hspec spec
