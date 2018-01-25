module ProgramSpec where
import Test.Hspec
import Compile
import System.Exit
import System.Process

flattenPath :: String -> String
flattenPath = map (\x -> if x == '/' then '_' else x)

run :: String -> Either String String -> IO Int
run fname instrs = let
  tmploc = "/tmp/" ++ flattenPath fname ++ ".S" in
    case instrs of
      (Left _) -> return (-1) -- Compilation error
      (Right s) -> do
         _ <- writeFile tmploc s
         _ <- callCommand $ "as " ++ tmploc ++ " -o " ++ tmploc ++ ".out"
         _ <- callCommand $ "ld " ++ tmploc ++ ".out -o /tmp/a.out"
         (exit, _, _) <- readProcessWithExitCode "/tmp/a.out" [] ""
         return (case exit of
           ExitSuccess -> 0
           ExitFailure e -> e)

{- Compile and run the file at the given path and return
   the return code
-}
compileAndRun :: String -> IO Int
compileAndRun s = do
  instrs <- compileFile s
  run s instrs

spec :: Spec
spec = do
  describe "Arithmetic" $ do
    it "should add numbers" $
      compileAndRun "samples/basic/add.al" `shouldReturn` 3
    it "should subtract numbers" $
      compileAndRun "samples/basic/sub.al" `shouldReturn` 3
    it "should divide numbers" $
      compileAndRun "samples/basic/div.al" `shouldReturn` 3
    it "should multiply numbers" $
      compileAndRun "samples/basic/mul.al" `shouldReturn` 3
  describe "Control flow" $ do
    it "If statements don't execute on false" $
      compileAndRun "samples/flow/if.al" `shouldReturn` 3
    it "If statements execute on true" $
      compileAndRun "samples/flow/if2.al" `shouldReturn` 3
    it "While statements work" $
      compileAndRun "samples/flow/while1.al" `shouldReturn` 3
  describe "Memory" $ do
    it "should compile arrays" $
      compileAndRun "samples/memory/arr.al" `shouldReturn` 3
  describe "Functions" $ do
    it "should compile functions with no args" $
      compileAndRun "samples/func/func.al" `shouldReturn` 3
    it "should compile functions with args" $
      compileAndRun "samples/func/func2.al" `shouldReturn` 3
  describe "Including" $ do
    it "should include files" $
      compileAndRun "samples/include/print.al" `shouldReturn` 0

main :: IO()
main = hspec spec
