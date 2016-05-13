module NodeSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Node

spec =
  describe "Prelude.head" $ do
    it "returns the first element of a list" $ do
      head [23 ..] `shouldBe` (23 :: Int)

