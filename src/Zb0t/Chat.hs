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

import Prelude ()
import Pregame

import Zb0t.Types
import Zb0t.Messager
import Zb0t.Logger

class Monad m => Chat m where
  login :: Nickname -> Maybe Password -> m ()
  enter :: Channel -> m ()
  leave :: Channel -> m ()
  say :: Medium -> Text -> m ()

login' :: (Messager m, Logger m) => Nickname -> Maybe Password -> m ()
login' nickname password' = do
  logInfo $ formatLoginLog nickname password'
  msg $ DetailNick nickname
  msg $ DetailUser (toUsername nickname) "0" "*" (toRealname nickname)
  whenMaybe password' (msg . DetailIdentity)

enter' :: (Messager m, Logger m) => Channel -> m ()
enter' channel = do
  logInfo $ formatEnterLog channel
  msg $ DetailJoin (channel :| [])

leave' :: (Messager m, Logger m) => Channel -> m ()
leave' channel = do
  logInfo $ formatLeaveLog channel
  msg $ DetailPart (channel :| [])

say' :: (Messager m, Logger m) => Medium -> Text -> m ()
say' target message = do
  logInfo $ formatSayLog target message
  msg $ DetailPrivate target message

formatLoginLog :: Nickname -> Maybe Password -> Text
formatLoginLog (Nickname nickname) password = "login: " <> nickname <> passwordStatus
  where
    passwordStatus = if isJust password then " with a password" else " without a password"

formatEnterLog :: Channel -> Text
formatEnterLog (Channel channel) = "enter: " <> channel

formatLeaveLog :: Channel -> Text
formatLeaveLog (Channel channel) = "leave: " <> channel

formatSayLog :: Medium -> Text -> Text
formatSayLog target message = "say: " <> target' <> " <- " <> message
  where
    target' = case target of
      MediumChannel (Channel channel) -> channel
      MediumNickname (Nickname nickname) -> nickname

whenMaybe :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenMaybe Nothing _ = return ()
whenMaybe (Just a) f = f a
