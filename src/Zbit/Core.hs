module Zbit.Core
  ( run
  ) where

----

import Zbit.Imports
import Zbit.Types

----

run :: Config -> IO ()
run (Config srv port chans nick mpswd) = withSocketsDo $
  do putStrLn ("server: " ++ srv)
     putStrLn ("port: " ++ show port)
     putStrLn ("chans: " ++ show chans)
     putStrLn ("nick: " ++ nick)
     putStrLn ("password: " ++ show mpswd)

