module Zb0t.Messager
  ( Messager(..)
  , msg'
  ) where

import Zb0t.Types
import Zb0t.Has

class Monad m => Messager m where
  msg :: Detail -> m ()

msg' :: Has m => Detail -> m ()
msg' message = do
  outbound <- getOutbound
  _writeMessageQueue outbound message
