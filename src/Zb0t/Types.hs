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
  , Medium(..)
  , Message(..)
  , Prefix(..)
  , Detail(..)
  , Response 
  , toUsername
  , toRealname
  ) where

import Prelude ()
import Pregame

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
  deriving (Show, Eq, IsString, ToText)

newtype Nickname = Nickname Text
  deriving (Show, Eq, IsString, ToText)

newtype Username = Username Text
   deriving (Show, Eq, IsString, ToText)

newtype Hostname = Hostname Text
   deriving (Show, Eq, IsString, ToText)

newtype Servername = Servername Text
  deriving (Show, Eq, IsString, ToText)

newtype Realname = Realname Text
  deriving (Show, Eq, IsString, ToText)

newtype Password = Password Text
  deriving (Show, Eq, IsString, ToText)

data Medium
  = MediumChannel Channel
  | MediumNickname Nickname
  deriving (Show, Eq)

instance ToText Medium where
  toText (MediumChannel channel) = toText channel
  toText (MediumNickname nickname) = toText nickname

data Message = Message
  { _prefix :: Prefix
  , _detail :: Detail
  } deriving (Show, Eq)

data Prefix
  = PrefixServername Servername
  | PrefixNickname Nickname (Maybe Username) (Maybe Servername)
  deriving (Show, Eq)

data Detail
  = DetailNick Nickname
  | DetailUser Username Hostname Servername Realname
  | DetailIdentity Password
  | DetailPrivate Medium Text
  | DetailJoin (NonEmpty Channel)
  | DetailPart (NonEmpty Channel)
  deriving (Show, Eq)

type Response = ()

toUsername :: Nickname -> Username
toUsername (Nickname x) = Username x

toRealname :: Nickname -> Realname
toRealname (Nickname x) = Realname x
