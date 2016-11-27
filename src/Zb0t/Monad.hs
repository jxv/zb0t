module Zb0t.Monad
  ( Zb0t
  , runIO
  ) where

import Protolude

import Zb0t.Types
import Zb0t.Has
import Zb0t.Chat
import Zb0t.Logger
import Zb0t.Messager

data Env = Env
  { _outbound :: MessageQueue Zb0t Detail
  }

newtype Zb0t a = Zb0t { unZb0t :: ReaderT Env IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env)

runIO :: Zb0t a -> Env -> IO a
runIO (Zb0t m) outbound = runReaderT m outbound

instance Has Zb0t where
  getOutbound = asks _outbound

instance Chat Zb0t where
  login = login'
  enter = enter'
  leave = leave'
  say = say' 

instance Logger Zb0t where
  logInfo = logInfo'

instance Messager Zb0t where
  msg = msg'
