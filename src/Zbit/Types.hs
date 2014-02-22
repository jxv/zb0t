module Zbit.Types where

----

import Zbit.Imports

----

data Config = Config
  { cfgServerAddr :: String
  , cfgServerPort :: Integer 
  , cfgChannels :: [String]
  , cfgNick :: String
  , cfgPassword :: Maybe String
  } deriving (Show, Eq)

