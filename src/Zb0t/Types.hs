module Zb0t.Types where


import qualified Network.IRC as IRC


data Config = Config
    { cfgServerAddr :: String
    , cfgServerPort :: Integer
    , cfgChannels :: [String]
    , cfgNick :: String
    , cfgPassword :: Maybe String
    } deriving (Show, Eq)


data Event 
    = Send IRC.Message 
    | Recv IRC.Message
    | RawMessage String
    deriving (Show, Eq)

