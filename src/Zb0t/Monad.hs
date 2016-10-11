module Zb0t.Monad
  ( Zb0t
  , runZb0t
  ) where

import Control.Monad.IO.Class (MonadIO(liftIO))

newtype Zb0t a = Zb0t { unZb0t :: IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

runZb0t :: Zb0t a -> IO a
runZb0t (Zb0t m) = m
