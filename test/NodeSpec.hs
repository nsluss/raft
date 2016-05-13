module NodeSpec where

import Node


import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

import TestOrphans

spec :: SpecWith ()
spec = do
  describe "an internal node's state" $ do
    describe "Getting a value from a single node" $ do
      it "should return the value of the node" $ do
        contents (NodeState 1) `shouldBe` (1 :: Int)

    describe "Typeclass laws for NodeState" $ do
      it "should satisfy all of them!" $ do
        quickBatch (functor     (undefined :: NodeState (String, String, String) ) )
        quickBatch (applicative (undefined :: NodeState (String, String, String) ) )
        quickBatch (monad       (undefined :: NodeState (String, String, String) ) )

  -- describe "A client-server relationship" $ do
  --   it "can have the client send a value to the server" $ do
  --     let x = 2
  --         client = pure x :: N
  --         toServer = pure (x + 1) :: N
  --         response = sendFrom client toServer
  --     evalNodeT response `shouldBe` (x :: IO Int)

