module Zb0t
  ( runIO
  , Zb0t
  , main
  , Log(..)
  , startMessage
  ) where

import Pregame

newtype Zb0t a = Zb0t (IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

runIO :: Zb0t a -> IO a
runIO (Zb0t m) = m

class Monad m => Log m where
  logInfo :: Text -> m ()

main :: Log m => m ()
main = do
  logInfo startMessage

startMessage :: Text
startMessage = "Starting..."

instance Log Zb0t where
  logInfo = liftIO . putStrLn
