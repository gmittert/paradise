module ProgramSpec where
import Test.Hspec
import Compile
import System.Exit
import System.Process
import Args

run :: String -> Either String String -> IO (Int, String, String)
run fname instrs = let
  tmploc = "/tmp/" ++ flattenPath fname ++ ".c" in
    case instrs of
      (Left _) -> return (-1, "", "") -- Compilation error
      (Right s) -> do
         writeFile tmploc s
         let args = CmdArgs False True "/tmp/a.out" ""
         cToExe tmploc args
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
      exitOf "samples/basic/add.para" `shouldReturn` 3
    it "should subtract numbers" $
      exitOf "samples/basic/sub.para" `shouldReturn` 3
    it "should divide numbers" $
      exitOf "samples/basic/div.para" `shouldReturn` 3
    it "should multiply numbers" $
      exitOf "samples/basic/mul.para" `shouldReturn` 3
  describe "Control flow" $ do
    it "If statements don't execute on false" $
      exitOf "samples/flow/if.para" `shouldReturn` 3
    it "If statements execute on true" $
      exitOf "samples/flow/if2.para" `shouldReturn` 3
    it "While statements work" $
      exitOf "samples/flow/while1.para" `shouldReturn` 3
  describe "Memory" $ do
    it "should compile arrays" $
      exitOf "samples/memory/arr.para" `shouldReturn` 3
    it "should compile arrays2" $
      exitOf "samples/memory/arr2.para" `shouldReturn` 3
    it "should compile byte arrays" $
      exitOf "samples/memory/arr3.para" `shouldReturn` 3
    it "should compile byte arrays" $
      exitOf "samples/memory/arr4.para" `shouldReturn` 3
  describe "Functions" $ do
    it "samples/func/func.para: should compile functions with no args" $
        exitOf "samples/func/func.para" `shouldReturn` 3
    it "samples/func/func2.para: should compile functions with args" $
      exitOf "samples/func/func2.para" `shouldReturn` 3
    it "samples/func/recursion.para: should compile functions with recursion" $
      exitOf "samples/func/recursion.para" `shouldReturn` 120
  describe "Including" $
    it "samples/func/print.para should include files" $
      exitOf "samples/include/print.para" `shouldReturn` 0
  describe "Strings" $
    it "samples/strings/str.pd should print strings" $
      stdoutOf "samples/strings/str.para" `shouldReturn` "Hello"
  describe "OpenCL" $
    it "samples/opencl/kernel1.para should square numbers" $
      exitOf "samples/opencl/kernel1.para" `shouldReturn` 3

main :: IO()
main = hspec spec
