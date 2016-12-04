module Zb0t.Logger
  ( Logger(..)
  , logInfo'
  ) where

import Pregame

class Monad m => Logger m where
  logInfo :: Text -> m ()

logInfo' :: MonadIO m => Text -> m ()
logInfo' = liftIO . putStrLn
