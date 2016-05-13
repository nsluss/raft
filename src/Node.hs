{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Node where

import Control.Monad.Trans.State.Lazy
import Control.Monad.State
import Control.Monad
import Data.Text (pack, Text)

data NodeState a = NodeState {
    status    :: NodeStatus
  , label     :: Text
  , contents  :: a
  } deriving (Eq, Ord, Show, Functor)

data NodeStatus = Follower | Candidate | Leader
  deriving (Eq, Ord, Show)

instance Applicative NodeState where
  pure a = NodeState Follower (pack $ show Follower ++ "Node") a
  fab <*> b = NodeState {
      status   = status b,
      label    = label b,
      contents = (contents fab $ contents b)
    }

instance Monad NodeState where
  return = pure
  a >>= famb = famb (contents a)

type Node a = State (NodeState a) (NodeState a)

evalNode :: Node a -> NodeState a -> NodeState a
evalNode cur init = ((evalState cur) init)

sendFrom :: Node a -> Node a -> Node a
sendFrom = const

