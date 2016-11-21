module Zb0t.Messager
  ( Messager(..)
  , msg'
  ) where

import Zb0t.Types
import Zb0t.Has

class Monad m => Messager m where
  msg :: Message -> m ()

msg' :: Has m => Message -> m ()
msg' message = do
  outbound <- getOutbound
  _writeMessageQueue outbound message
