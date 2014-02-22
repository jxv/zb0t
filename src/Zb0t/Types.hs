module Zb0t.Types where

----

import Zb0t.Imports

----

data Config = Config
  { cfgServerAddr :: String
  , cfgServerPort :: Integer
  , cfgChannels :: [String]
  , cfgNick :: String
  , cfgPassword :: Maybe String
  } deriving (Show, Eq)

data Event 
  = Send Message 
  | Recv Message
  | RawMessage String

