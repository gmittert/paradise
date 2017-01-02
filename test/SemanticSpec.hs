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
moddedBar :: CodegenState
moddedBar = CodegenState {
  symTab = SymbolTable {
     vars = M.insert (Name "bar") (Entry Int (Addr 8)) M.empty
     , funcs = M.empty
  }
  , offset = 8
  , nextTmp = 0
  }
shadow :: CodegenState
shadow = CodegenState {
  symTab = SymbolTable {
     vars = M.insert (Name "foo") (Entry Bool (Addr 1)) M.empty
     , funcs = M.empty
  }
  , offset = 1
  , nextTmp = 0
  }

moddedAll :: CodegenState
moddedAll = CodegenState {
  symTab = SymbolTable {
     vars = M.insert (Name "test") (Entry Int (Addr 16)) $
       M.insert (Name "bar") (Entry Int (Addr 8)) M.empty
     , funcs = M.empty
  }
  , offset = 16
  , nextTmp = 0
  }

moddedAllShadow :: CodegenState
moddedAllShadow = CodegenState {
  symTab = SymbolTable {
     vars = M.insert (Name "foo") (Entry Int (Addr 16)) M.empty
     , funcs = M.empty
     }
  , offset = 16
  , nextTmp = 0
  }

op :: Expr
op = Op Plus (Name "test") (Lit 0 emptyState) emptyState

decl :: Statement
decl = SDecl (Name "foo") Int emptyState

declTest :: Statement
declTest = SDecl (Name "test") Int emptyState

voidNoneBlock :: VoidBlock
voidNoneBlock = Void None (Statements' decl emptyState) emptyState

voidEmptyArgsBlock :: VoidBlock
voidEmptyArgsBlock = Void (Args []) (Statements' decl emptyState) emptyState

voidArgsBlock :: VoidBlock
voidArgsBlock = Void (Args [Arg Int (Name "bar")]) (Statements' decl emptyState) emptyState

voidDupArgsBlock :: VoidBlock
voidDupArgsBlock = Void (Args [Arg Bool (Name "foo")]) (Statements' decl emptyState) emptyState

retNoneBlock :: RetBlock
retNoneBlock = Ret None (Statements' declTest emptyState) op emptyState

retEmptyArgsBlock :: RetBlock
retEmptyArgsBlock = Ret (Args []) (Statements' declTest emptyState) op emptyState

retArgsBlock :: RetBlock
retArgsBlock = Ret (Args [Arg Int (Name "bar")]) (Statements' declTest emptyState) op emptyState

retShadowBlock :: RetBlock
retShadowBlock = Ret (Args [Arg Int (Name "foo")]) (Statements' decl emptyState) op emptyState

retDupArgsBlock :: RetBlock
retDupArgsBlock = Ret (Args [Arg Bool (Name "foo")]) (Statements' declTest emptyState) op emptyState

declAssign :: Statement
declAssign = SDeclAssign (Name "foo") Int op emptyState

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
  describe "when analyzing a void block" $ do
    it "should not modify the scope" $ do
      getState initVoidBlock voidNoneBlock modded `shouldBe` modded
      getState initVoidBlock voidArgsBlock modded `shouldBe` modded
      getState initVoidBlock voidEmptyArgsBlock modded `shouldBe` modded
      getState initVoidBlock voidDupArgsBlock modded `shouldBe` modded
    it "should pass the scope to statements inside if there is no argument block" $
      let (Void _ (Statements' _ table) _) = evalState (genCode (initVoidBlock voidNoneBlock)) modded in
        table `shouldBe` modded
    it "should pass only the arguments to statements inside if there is an argument block" $
      let (Void _ (Statements' _ table) _) = evalState (genCode (initVoidBlock voidArgsBlock)) modded in
        table `shouldBe` moddedBar
    it "should pass the empty scope to the statements inside if there is an empty argument block" $
      let (Void _ (Statements' _ table) _) = evalState (genCode (initVoidBlock voidEmptyArgsBlock)) modded in
        table `shouldBe` empty
    it "should shadow inherited scope if there are duplicate names" $
      let (Void _ (Statements' _ table) _) = evalState (genCode (initVoidBlock voidDupArgsBlock)) modded in
        table `shouldBe` shadow
  describe "when analyzing a return block" $ do
    it "should not modify the scope" $ do
      getState initRetBlock retNoneBlock modded `shouldBe` modded
      getState initRetBlock retArgsBlock modded `shouldBe` modded
      getState initRetBlock retEmptyArgsBlock modded `shouldBe` modded
      getState initRetBlock retDupArgsBlock modded `shouldBe` modded
    it "should pass the scope to statements inside if there is no argument block" $
      let (Ret _ (Statements' _ table) _ _) = evalState (genCode (initRetBlock retNoneBlock)) modded in
        table `shouldBe` modded
    it "should pass only the arguments to statements inside if there is an argument block" $
      let (Ret _ (Statements' _ table) _ _) = evalState (genCode (initRetBlock retArgsBlock)) modded in
        table `shouldBe` moddedBar
    it "should pass the empty scope to the statements inside if there is an empty argument block" $
      let (Ret _ (Statements' _ table) _ _) = evalState (genCode (initRetBlock retEmptyArgsBlock)) modded in
        table `shouldBe` empty
    it "should shadow inherited scope if there are duplicate names" $
      let (Ret _ (Statements' _ table) _ _) = evalState (genCode (initRetBlock retDupArgsBlock)) modded in
        table `shouldBe` shadow
    it "should pass the scope from the statements to the return expression" $
      let (Ret _ _ (Op _ _ _ table) _) = evalState (genCode (initRetBlock retArgsBlock)) modded in
        table `shouldBe` moddedAll
    it "should shadow scopes as the are passed to the expression " $
      let (Ret _ _ (Op _ _ _ table) _) = evalState (genCode (initRetBlock retShadowBlock)) modded in
        table `shouldBe` moddedAllShadow
