module SemanticSpec where
import Semantic
import Syntax
import Types
import Control.Monad.State.Lazy
import qualified Data.Map as M
import Test.Hspec

empty :: CodegenState
empty = CodegenState emptyTable 0 0

modded :: CodegenState
modded = CodegenState {
  symTab = SymbolTable {
     vars = M.insert (Name "foo") (Entry Int (Addr 8)) M.empty
     , funcs = M.empty
  }
  , offset = 8
  , nextTmp = 0
  }

op :: Expr
op = Op Plus (Name "test") (Lit 0 emptyState) emptyState

decl :: Statement
decl = SDecl (Name "foo") Int emptyState

declTest :: Statement
declTest = SDecl (Name "test") Int emptyState

declAssign :: Statement
declAssign = SDeclAssign (Name "foo") Int op emptyState

declExpr :: Statements
declExpr = Statements (Statements' (SDecl (Name "foo") Int emptyState) emptyState) (SExpr op emptyState) emptyState

getState :: (a -> CodeGen a) -> a -> CodegenState -> CodegenState
getState f a = execState (genCode $ f a)

spec :: Spec
spec = do
  describe "when analyzing an expression" $
    it "should not modify the scope" $
      getState initExpr op modded `shouldBe` modded
  describe "when analyzing a declaration" $
    it "should add the variable to the scope" $
      getState initStatement decl empty `shouldBe` modded
  describe "when analyzing a declaration assignment" $
    it "should add the variable to the scope" $
      getState initStatement declAssign empty `shouldBe` modded
