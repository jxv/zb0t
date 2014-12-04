{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Zb0t.Config where

import Control.Lens
import Data.Default

data Config = Config
    { _serverAddr :: String
    , _serverPort :: Int
    , _channels :: [String]
    , _nick :: String
    , _password :: Maybe String
    } deriving (Show, Eq)

makeLenses ''Config

instance Default Config where
    def = Config "" 6667 [] "" Nothing
