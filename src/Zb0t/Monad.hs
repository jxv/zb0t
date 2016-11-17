module Zb0t.Monad
  ( Zb0t
  , runIO
  ) where

import Control.Monad.IO.Class (MonadIO(liftIO))

newtype Zb0t a = Zb0t { unZb0t :: IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

runIO :: Zb0t a -> IO a
runIO (Zb0t m) = m
