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
  describe "samples/basic/test.c" $
    it "should add numbers" $
      compileAndRun "samples/basic/test.c" `shouldReturn` 3

main :: IO()
main = hspec spec
