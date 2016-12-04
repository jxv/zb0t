module Zb0t.Control
  ( ControlM
  , runIO
  , main
  , startMessage
  ) where

import Pregame
import Zb0t.Logger

main :: Logger m => m ()
main = do
  logInfo startMessage

startMessage :: Text
startMessage = "Starting..."

newtype ControlM a = ControlM (IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

runIO :: ControlM a -> IO a
runIO (ControlM m) = m

instance Logger ControlM where
  logInfo = logInfo'
