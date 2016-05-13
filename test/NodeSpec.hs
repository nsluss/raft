module NodeSpec where

import Node

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

spec =
  describe "Getting a value from a single node" $ do
    it "should return the value of the node" $ do
      runNode (Node 1) `shouldBe` (1 :: Int)

