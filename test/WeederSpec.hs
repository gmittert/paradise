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
                   \I64 add(I64 x, I64 x) {return 0;}\n\
                   \I64 main(){return add(1,2);}\n"
       in do
         res <- compileString prog
         res `shouldSatisfy` isError

main :: IO()
main = hspec spec
