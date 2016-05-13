module NodeSpec where

import Node

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

import TestOrphans
import Control.Monad.Trans.State.Lazy

spec :: SpecWith ()
spec = do
  describe "an internal node's state" $ do
    describe "Getting a value from a single node" $ do
      it "should return the value of the node" $ do
        contents (NodeState Follower 1) `shouldBe` (1 :: Int)

    describe "Typeclass laws for NodeState" $ do
      it "should satisfy all of them!" $ do
        quickBatch (functor     (undefined :: NodeState (String, String, String) ) )
        quickBatch (applicative (undefined :: NodeState (String, String, String) ) )
        quickBatch (monad       (undefined :: NodeState (String, String, String) ) )

    describe "Initializing a node" $ do
      it "starts it as a Follower" $ do
        status (pure 2 :: NodeState Int) `shouldBe` (Follower :: NodeStatus)

  describe "A client" $ do
    it "can send a server a value" $ do
      let x = 2
          cState = NodeState Follower x
          sState = (+1) <$> cState
          client      = return cState :: Node Int
          toServer    = return sState :: Node Int
          newServer   = sendFrom client toServer
      contents (evalNode newServer sState) `shouldBe` (x :: Int)

