module Zbit.Config
  ( defPort
  , defConfig
  , makeConfig
  ) where

----

import Zbit.Imports
import Zbit.Types

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
parseServerAddr [] = put (Left "no server address")
parseServerAddr ("-s":addr:_) = getput $ \cfg -> cfg {cfgServerAddr = addr}
parseServerAddr (_:args) = parseServerAddr args 


parseServerPort :: [String] -> State (Either String Config) ()
parseServerPort [] = return ()
parseServerPort ("-p":sport:_) = getput $ \cfg -> cfg {cfgServerPort = fromMaybe defPort (readMay sport)}
parseServerPort ("-c":_) = return ()
parseServerPort (_:args) = parseServerPort args 


parseChannels :: [String] -> State (Either String Config) ()
parseChannels [] = return ()
parseChannels ("-c":chans) = getput $ \cfg -> cfg {cfgChannels = chans}
parseChannels (_:args) = parseChannels args


parseNick :: [String] -> State (Either String Config) ()
parseNick [] = put (Left "no nick")
parseNick ("-n":nick:_) = getput $ \cfg -> cfg {cfgNick = nick}
parseNick ("-c":_) = put (Left "no nick")
parseNick (_:args) = parseNick args


parsePassword :: [String] -> State (Either String Config) ()
parsePassword [] = return ()
parsePassword ("-w":pswd:_) = getput $ \cfg -> cfg {cfgPassword = Just pswd}
parsePassword ("-c":_) = return ()
parsePassword (_:args) = parsePassword args 

