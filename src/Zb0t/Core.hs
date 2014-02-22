module Zb0t.Core
  ( run
  ) where

----

import Zb0t.Imports
import Zb0t.Types

----

getConn :: Config -> IO (Maybe Handle)
getConn (Config srv port _ _ _) =
  do let conn = connectTo srv (PortNumber $ fromIntegral port)
     (fmap Just conn) `catchIOError` (const $ return Nothing)

----

run :: Config -> IO ()
run cfg = withSocketsDo $
  do mconn <- getConn cfg
     case mconn of
       Nothing -> putStrLn "No connection."
       Just conn ->
         do chan <- newChan
            _ <- forkIO (recv cfg conn chan)
            (send cfg conn chan)

----

recv :: Config -> Handle -> Chan Event -> IO ()
recv _ conn chan = fix $ \loop ->
  do again <- (recvMsg conn chan) `catchIOError` (const $ return False)
     when again loop

recvMsg :: Handle -> Chan Event -> IO Bool 
recvMsg conn _ =
  do line <- hGetLine conn
     putStrLn line
     return True

----

send :: Config -> Handle -> Chan Event -> IO ()
send _ conn chan = fix $ \loop ->
  do again <- (sendMsg conn chan) `catchIOError` (const $ return False)
     when again loop

sendMsg :: Handle -> Chan Event -> IO Bool
sendMsg conn _ =
  do line <- getLine
     hPutStr conn  (line ++ "\n")
     return True

{-

NICK <nick>
USER <nick> 0 * :<nick>
PRIVMSG nickserv :identify <password>


PING
PONG

PRIVMSG <channel> :msg

-}

