{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Zb0t.Config where

import Control.Lens
import Data.Default
import Data.ByteString (ByteString)

data Config = Config
    { _serverAddr :: ByteString
    , _serverPort :: Int
    , _channels :: [ByteString]
    , _nick :: ByteString
    , _password :: Maybe ByteString
    } deriving (Show, Eq)

makeLenses ''Config

instance Default Config where
    def = Config "" 6667 [] "" Nothing
