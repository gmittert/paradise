module ProgramSpec where
import Test.Hspec
import Compile
import System.Exit
import System.Process

run :: String -> Either String String -> IO (Int, String, String)
run fname instrs = let
  tmploc = "/tmp/" ++ flattenPath fname ++ ".S" in
    case instrs of
      (Left _) -> return (-1, "", "") -- Compilation error
      (Right s) -> do
         writeFile tmploc s
         makeExecutable "/tmp/a.out" [tmploc]
         (exit, stdout, stderr) <- readProcessWithExitCode "/tmp/a.out" [] ""
         return ((case exit of
           ExitSuccess -> 0
           ExitFailure e -> e), stdout, stderr)

exitOf :: String -> IO Int
exitOf fname = do
  instrs <- compileFile fname
  run fname instrs >>= (\(a, _, _) -> return a)

stdoutOf :: String -> IO String
stdoutOf fname = do
  instrs <- compileFile fname
  run fname instrs >>= (\(_, a, _) -> return a)

stderrOf :: String -> IO String
stderrOf fname = do
  instrs <- compileFile fname
  run fname instrs >>= (\(_, _, a) -> return a)

spec :: Spec
spec = do
  describe "Arithmetic" $ do
    it "should add numbers" $
      exitOf "samples/basic/add.al" `shouldReturn` 3
    it "should subtract numbers" $
      exitOf "samples/basic/sub.al" `shouldReturn` 3
    it "should divide numbers" $
      exitOf "samples/basic/div.al" `shouldReturn` 3
    it "should multiply numbers" $
      exitOf "samples/basic/mul.al" `shouldReturn` 3
  describe "Control flow" $ do
    it "If statements don't execute on false" $
      exitOf "samples/flow/if.al" `shouldReturn` 3
    it "If statements execute on true" $
      exitOf "samples/flow/if2.al" `shouldReturn` 3
    it "While statements work" $
      exitOf "samples/flow/while1.al" `shouldReturn` 3
  describe "Memory" $ do
    it "should compile arrays" $
      exitOf "samples/memory/arr.al" `shouldReturn` 3
    it "should compile arrays2" $
      exitOf "samples/memory/arr2.al" `shouldReturn` 3
  describe "Functions" $ do
    it "samples/func/func.al: should compile functions with no args" $
        exitOf "samples/func/func.al" `shouldReturn` 3
    it "samples/func/func2.al: should compile functions with args" $
      exitOf "samples/func/func2.al" `shouldReturn` 3
    it "samples/func/recursion.al: should compile functions with recursion" $
      exitOf "samples/func/recursion.al" `shouldReturn` 120
  describe "Including" $
    it "samples/func/print.al should include files" $
      exitOf "samples/include/print.al" `shouldReturn` 0
  describe "Strings" $
    it "samples/strings/str.al should print strings" $
      stdoutOf "samples/strings/str.al" `shouldReturn` "Hello"

main :: IO()
main = hspec spec
