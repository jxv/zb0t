module Zb0t.Has
  ( Has(..)
  ) where

import Zb0t.Types

class Monad m => Has m where
  getOutbound :: m (MessageQueue m Message)
