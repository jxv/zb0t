{-# LANGUAGE OverloadedStrings #-}

module Zb0t.Core
  ( run
  ) where

----

import Zb0t.Imports
import Zb0t.Types
import Zb0t.Say (zsay)

----

getConn :: Config -> IO (Maybe Handle)
getConn (Config srv port _ _ _) =
  do let conn = connectTo srv (PortNumber $ fromIntegral port)
     (fmap Just conn) `catchIOError` (const $ return Nothing)

----

run :: Config -> IO ()
run cfg@(Config _ _ chans nck mpswd) = withSocketsDo $
  do mconn <- getConn cfg
     case mconn of
       Nothing -> putStrLn "No connection."
       Just conn ->
         do chan <- newChan
            login nck mpswd chan
            join' chans chan
            thdrecv <- forkIO (recv conn chan)
            thdinput <- forkIO (input chan)
            send cfg conn chan
            killThread thdrecv
            killThread thdinput

----

input :: Chan Event -> IO ()
input chan =
  do line <- getLine
     writeChan chan (RawMessage line)
     input chan

----

login :: String -> Maybe String -> Chan Event -> IO ()
login nck mpswd chan =
  do writeChan chan (Send $ nickMsg nck)
     writeChan chan (Send $ userMsg nck 0 "*" nck)
     case mpswd of 
        Nothing -> return ()
        Just pswd -> writeChan chan (Send $ identifyMsg pswd)

join' :: [String] -> Chan Event -> IO ()
join' chans chan = mapM_ (writeChan chan . Send . joinMsg) chans

recv :: Handle -> Chan Event -> IO ()
recv conn chan =
  do again <- (recvMsg conn chan) `catchIOError` (const $ return False)
     when again (recv conn chan)

recvMsg :: Handle -> Chan Event -> IO Bool 
recvMsg conn chan =
  do line <- hGetLine conn
     putStrLn line
     case (decode $ toBString line) of
       Nothing -> return ()
       Just msg -> writeChan chan (Recv msg)
     return True

----

send :: Config -> Handle -> Chan Event -> IO ()
send cfg conn chan = 
  do again <- (sendMsg cfg conn chan) `catchIOError` (const $ return False)
     when again (send cfg conn chan)

sendMsg :: Config -> Handle -> Chan Event -> IO Bool
sendMsg cfg conn chan =
  do event <- readChan chan
     case event of
       Send msg -> do let m = toString (encode msg)
                      putStrLn m
                      hPutStrLn conn m
                      return True
       Recv msg -> replyMsg cfg conn msg >> return True
       RawMessage msg -> hPutStrLn conn msg >> putStrLn msg >> return True

replyMsg :: Config -> Handle -> Message -> IO ()
replyMsg _ conn (Message Nothing cmd params)
  | cmd == "PING" = reply "PONG"
  | otherwise = return ()
  where reply x = hPutStrLn conn x >> putStrLn x
replyMsg (Config _ _ _ nck _) conn (Message (Just (NickName sender _ _)) cmd (recvr:msg))
  | cmd == "PRIVMSG" && prefixWith "zsay " (toString (head msg)) =
       reply $ "PRIVMSG " ++
               toString (if toString recvr == nck then sender else recvr) ++
               " :" ++
               (zsay . drop 5 . unwords . map toString $ msg)
  | otherwise = return ()
  where reply x = hPutStrLn conn x >> putStrLn x
replyMsg _ _ _ = return ()

prefixWith :: String -> String -> Bool
prefixWith xs ys = and $ zipWith (==) xs ys

----

nickMsg :: String -> Message
nickMsg n = Message Nothing "NICK" [toBString n]

userMsg :: String -> Int -> String -> String -> Message
userMsg u m t n = Message Nothing "USER" (map toBString [u, show m, t, ' ':n])

identifyMsg :: String -> Message
identifyMsg p = Message Nothing "PRIVMSG" ["nickserv", ":identify", toBString p]

zmsg :: String -> String -> Message
zmsg target msg = Message Nothing "PRIVMSG" (map toBString [target, ' ':(zsay msg)])

pongMsg :: Message
pongMsg = Message Nothing "PONG" []

joinMsg :: String -> Message
joinMsg chan = Message Nothing "JOIN" [toBString chan]

----

toString :: ByteString -> String
toString = map (toEnum . fromEnum) . unpack

toBString :: String -> ByteString
toBString = pack . map (toEnum . fromEnum)

