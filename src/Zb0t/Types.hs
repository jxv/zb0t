module Zb0t.Types
  ( Event(..)
  ) where

import qualified Network.IRC as IRC

data Event 
  = Send IRC.Message 
  | Recv IRC.Message
  | RawMessage String
  deriving (Show, Eq)
