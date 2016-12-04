module Zb0t
  ( runIO
  ) where

import Pregame
import qualified Zb0t.Control as Control

runIO :: IO ()
runIO = do
  Control.runIO Control.main

class Monad m => Log m where
  logInfo :: Text -> m ()
