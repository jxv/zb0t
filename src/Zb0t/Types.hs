module Zb0t.Types
  ( MessageQueue(..)
  , Event(..)
  , Channel(..)
  , Nickname(..)
  , Username(..)
  , Hostname(..)
  , Servername(..)
  , Realname(..)
  , Password(..)
  , Reciever(..)
  , Message(..)
  , Response 
  , toUsername
  , toRealname
  ) where

import Protolude
import Data.List.NonEmpty (NonEmpty)
import qualified Network.IRC as IRC

data Event 
  = Send IRC.Message 
  | Recv IRC.Message
  | RawMessage String
  deriving (Show, Eq)

---------------

data MessageQueue m a = MessageQueue
  { _writeMessageQueue :: a -> m ()
  }

newtype Channel = Channel Text
  deriving (Show, Eq, IsString)

newtype Nickname = Nickname Text
  deriving (Show, Eq, IsString)

newtype Username = Username Text
   deriving (Show, Eq, IsString)

newtype Hostname = Hostname Text
   deriving (Show, Eq, IsString)

newtype Servername = Servername Text
  deriving (Show, Eq, IsString)

newtype Realname = Realname Text
  deriving (Show, Eq, IsString)

newtype Password = Password Text
  deriving (Show, Eq, IsString)

data Reciever
  = RecieverChannel Channel
  | RecieverNickname Nickname
  deriving (Show, Eq)

data Message
  = MessageNick Nickname
  | MessageUser Username Hostname Servername Realname
  | MessageIdentity Password
  | MessagePrivate (NonEmpty Reciever) Text
  | MessageJoin (NonEmpty Channel)
  | MessagePart (NonEmpty Channel)
  deriving (Show, Eq)

type Response = ()

toUsername :: Nickname -> Username
toUsername (Nickname x) = Username x

toRealname :: Nickname -> Realname
toRealname (Nickname x) = Realname x
