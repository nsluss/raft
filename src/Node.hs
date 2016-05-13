{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Node where

import Control.Monad.Trans.State.Lazy
import Control.Monad.State
import Control.Monad

newtype NodeState a = NodeState { contents :: a }
  deriving (Eq, Ord, Show, Functor)

instance Applicative NodeState where
  pure a = NodeState a
  (NodeState fab) <*> (NodeState a) = NodeState (fab a)

instance Monad NodeState where
  return = pure
  (NodeState a) >>= famb = famb a

type Node a = State (NodeState a) (NodeState a)

evalNode :: Node a -> NodeState a -> NodeState a
evalNode cur init = ((evalState cur) init)

sendFrom :: Node a -> Node a -> Node a
sendFrom = const

