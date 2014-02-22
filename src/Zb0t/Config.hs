module Zb0t.Config
  ( defPort
  , defConfig
  , makeConfig
  ) where

----

import Zb0t.Imports
import Zb0t.Types

----

getput :: (Monad m) => (a -> a) -> State (m a) ()
getput f = get >>= \mst -> put (mst >>= \st -> return $ f st)

----

defPort :: (Integral a) => a
defPort = 6667

defConfig :: Config
defConfig = Config "" defPort [] "" Nothing

----

makeConfig :: [String] -> Either String Config
makeConfig args =
  let parsers = [parseServerAddr, parseServerPort, parseChannels, parseNick, parsePassword]
      parse = foldl1 (>>) (map ($ args) parsers)
  in execState parse (Right defConfig)

----

parseServerAddr :: [String] -> State (Either String Config) ()
parseServerAddr x = case x of
  [] -> put (Left "No server address.")
  ("-s":addr:_) -> getput $ \cfg -> cfg {cfgServerAddr = addr}
  (_:args) -> parseServerAddr args 

parseServerPort :: [String] -> State (Either String Config) ()
parseServerPort x = case x of
  [] -> return ()
  ("-p":sport:_) -> getput $ \cfg -> cfg {cfgServerPort = fromMaybe defPort (readMay sport)}
  ("-c":_) -> return ()
  (_:args) -> parseServerPort args 

parseChannels :: [String] -> State (Either String Config) ()
parseChannels x = case x of
  [] -> return ()
  ("-c":chans) -> getput $ \cfg -> cfg {cfgChannels = chans}
  (_:args) -> parseChannels args

parseNick :: [String] -> State (Either String Config) ()
parseNick x = case x of
  [] -> put (Left "No nick.")
  ("-n":nick:_) -> getput $ \cfg -> cfg {cfgNick = nick}
  ("-c":_) -> put (Left "No nick.")
  (_:args) -> parseNick args

parsePassword :: [String] -> State (Either String Config) ()
parsePassword x = case x of
  [] -> return ()
  ("-w":pswd:_) -> getput $ \cfg -> cfg {cfgPassword = Just pswd}
  ("-c":_) -> return ()
  (_:args) -> parsePassword args 

