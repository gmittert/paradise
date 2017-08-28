module ProgramSpec where
import Test.Hspec
import Compile
import Control.Monad
import System.Exit
import System.Process

flattenPath :: String -> String
flattenPath = map (\x -> if x == '/' then '_' else x)

run :: String -> Either String String -> IO Int
run fname instrs = let
  tmploc = "/tmp/" ++ flattenPath fname ++ ".S" in
    case instrs of
      (Left s) -> return (-1) -- Compilation error
      (Right s) -> do
         _ <- writeFile tmploc s
         _ <- callCommand $ "gcc " ++ tmploc ++ " -o " ++ tmploc ++ ".out"
         (exit, _, _) <- readProcessWithExitCode (tmploc ++ ".out") [] ""
         return (case exit of
           ExitSuccess -> 0
           ExitFailure e -> e)

{- Compile and run the file at the given path and return
   the return code
-}
compileAndRun :: String -> IO Int
compileAndRun s = let
  instrs = process <$> readFile s
  in join $ (run s) <$> instrs

spec :: Spec
spec = do
  describe "Arithmetic" $ do
    it "should add numbers" $
      compileAndRun "samples/basic/add.c" `shouldReturn` 3
    it "should subtract numbers" $
      compileAndRun "samples/basic/sub.c" `shouldReturn` 3
    it "should divide numbers" $
      compileAndRun "samples/basic/div.c" `shouldReturn` 3
    it "should multiply numbers" $
      compileAndRun "samples/basic/mul.c" `shouldReturn` 3
  describe "Control flow" $ do
    it "If statements don't execute on false" $
      compileAndRun "samples/flow/if.c" `shouldReturn` 3
    it "If statements execute on true" $
      compileAndRun "samples/flow/if2.c" `shouldReturn` 3
    it "While statements work" $
      compileAndRun "samples/flow/while1.c" `shouldReturn` 3
  describe "Memory" $ do
    it "should compile arrays" $
      compileAndRun "samples/memory/arr.c" `shouldReturn` 3
  describe "Functions" $ do
    it "should compile functions with no args" $
      compileAndRun "samples/func/func.c" `shouldReturn` 3
    it "should compile functions with args" $
      compileAndRun "samples/func/func2.c" `shouldReturn` 3

main :: IO()
main = hspec spec
