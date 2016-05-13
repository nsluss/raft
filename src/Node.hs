{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Node where

import Control.Monad.Trans.State.Lazy
import Control.Monad.State
import Control.Monad

data NodeState a = NodeState {
    status    :: NodeStatus
  , contents  :: a
  } deriving (Eq, Ord, Show, Functor)

data NodeStatus = Follower | Candidate | Leader
  deriving (Eq, Ord, Show)

instance Applicative NodeState where
  pure a = NodeState Follower a
  (NodeState _ fab) <*> (NodeState s a) = NodeState s (fab a)

instance Monad NodeState where
  return = pure
  (NodeState _ a) >>= famb = famb a

type Node a = State (NodeState a) (NodeState a)

evalNode :: Node a -> NodeState a -> NodeState a
evalNode cur init = ((evalState cur) init)

sendFrom :: Node a -> Node a -> Node a
sendFrom = const

