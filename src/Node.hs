module Node (
    Node(..)
  , runNode
  ) where

import Data.Functor ()
import Control.Monad ()
import Control.Applicative ()

data Node a = Node a

runNode :: Node a -> a
runNode (Node a) = a

instance Functor Node where
  fmap f (Node a) = Node (f a)

instance Applicative Node where
  pure a = Node a
  (Node fab) <*> (Node b) = Node (fab b)

instance Monad Node where
  return = pure
  (Node a) >>= faNb = faNb a

