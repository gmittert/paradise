{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module GenCState where
import qualified Data.Set as S
import Lib.Types

import Control.Monad.State.Lazy

data GenCState a
  = GenCState {
    -- | We track the structs that we use since we have to define them
    defs :: S.Set a
    -- | We track the variables we allocate so we can free them at the end
    -- of the block. We maintain a stack which we grow and shrink as we enter
    -- and exit scopes
    , alloced :: [[String]]
    , tmp :: Int
    }
  deriving (Eq, Ord, Show)
emptyState :: GenCState a
emptyState = GenCState S.empty [] 0

newtype (GenC b) a = GenC { genC :: State (GenCState b) a }
  deriving (Functor, Applicative, Monad, MonadState (GenCState b))

newScope :: GenC a ()
newScope = modify $ \s -> s { alloced = [] : alloced s }

exitScope :: GenC a ()
exitScope = modify $ \s -> s { alloced = tail $ alloced s }

newTmp :: GenC a Name
newTmp = do
  v <- tmp <$> get
  modify $ \s -> s { tmp = 1 + tmp s }
  return . Name $ "Ctmp" ++ show v

