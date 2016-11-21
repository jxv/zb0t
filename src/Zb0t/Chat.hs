{-# LANGUAGE OverloadedStrings #-}
module Zb0t.Chat
  ( Chat(..)
  , login'
  , enter'
  , leave'
  , say'
  , formatLoginLog
  , formatEnterLog
  , formatLeaveLog
  , formatSayLog
  , whenMaybe
  ) where

import Protolude hiding (msg)
import Data.List.NonEmpty (NonEmpty(..))

import Zb0t.Types
import Zb0t.Messager
import Zb0t.Logger

class Monad m => Chat m where
  login :: Nickname -> Maybe Password -> m ()
  enter :: Channel -> m ()
  leave :: Channel -> m ()
  say :: Reciever -> Text -> m ()

login' :: (Messager m, Logger m) => Nickname -> Maybe Password -> m ()
login' nickname password' = do
  logInfo $ formatLoginLog nickname password'
  msg $ MessageNick nickname
  msg $ MessageUser (toUsername nickname) "0" "*" (toRealname nickname)
  whenMaybe password' (msg . MessageIdentity)

enter' :: (Messager m, Logger m) => Channel -> m ()
enter' channel = do
  logInfo $ formatEnterLog channel
  msg $ MessageJoin (channel :| [])

leave' :: (Messager m, Logger m) => Channel -> m ()
leave' channel = do
  logInfo $ formatLeaveLog channel
  msg $ MessagePart (channel :| [])

say' :: (Messager m, Logger m) => Reciever -> Text -> m ()
say' reciever message = do
  logInfo $ formatSayLog reciever message
  msg $ MessagePrivate (reciever :| []) message

formatLoginLog :: Nickname -> Maybe Password -> Text
formatLoginLog (Nickname nickname) password = "login: " <> nickname <> passwordStatus
  where
    passwordStatus = if isJust password then " with a password" else " without a password"

formatEnterLog :: Channel -> Text
formatEnterLog (Channel channel) = "enter: " <> channel

formatLeaveLog :: Channel -> Text
formatLeaveLog (Channel channel) = "leave: " <> channel

formatSayLog :: Reciever -> Text -> Text
formatSayLog reciever message = "say: " <> target <> " <- " <> message
  where
    target = case reciever of
      RecieverChannel (Channel channel) -> channel
      RecieverNickname (Nickname nickname) -> nickname

whenMaybe :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenMaybe Nothing _ = return ()
whenMaybe (Just a) f = f a
