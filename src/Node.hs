{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Node where

import Control.Monad.Trans.State.Lazy
import Control.Monad.State
import Control.Monad
import Data.Text (pack, Text)
import Control.Lens

data NodeState a = NodeState {
    _status    :: NodeStatus
  , _label    :: Text
  , _contents :: a
  } deriving (Eq, Ord, Show, Functor)

data NodeStatus = Follower | Candidate | Leader
  deriving (Eq, Ord, Show)

$(makeLenses ''NodeState)

instance Applicative NodeState where
  pure a = NodeState Follower (pack $ show Follower ++ "Node") a
  fab <*> b = over contents (view contents fab) b

instance Monad NodeState where
  return = pure
  a >>= famb = famb (view contents a)

type Node a = State (NodeState a) (NodeState a)

evalNode :: Node a -> NodeState a -> NodeState a
evalNode cur init = ((evalState cur) init)

sendFrom :: Node a -> Node a -> Node a
sendFrom = const

