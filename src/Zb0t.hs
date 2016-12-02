module Zb0t
  ( runIO
  , Zb0t
  , main
  ) where

import Pregame

newtype Zb0t a = Zb0t (IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

runIO :: Zb0t a -> IO a
runIO (Zb0t m) = m

main :: Monad m => m ()
main = return ()
