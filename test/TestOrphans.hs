module TestOrphans where

import Node (NodeState(..), NodeStatus(..))
import Test.QuickCheck
import Test.QuickCheck.Checkers

instance Eq a => EqProp (NodeState a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (NodeState a) where
  arbitrary = NodeState Follower <$> arbitrary



