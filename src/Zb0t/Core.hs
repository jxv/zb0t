{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Zb0t.Core
    ( run
    ) where

import           Control.Monad (when)
import           Control.Applicative hiding ((<|>))
import           GHC.IO.Handle (Handle)
import           System.IO (hPutStrLn,hGetLine)
import           System.IO.Error (catchIOError)
import qualified Control.Concurrent as Conc
import qualified Control.Concurrent.Chan as Conc
import qualified Network as Network
import qualified Network.IRC as IRC
import qualified Data.ByteString as BS
import qualified System.Random as Random
import qualified Text.Parsec as Parsec
import           Text.Parsec ((<|>))


import Zb0t.Types
import Zb0t.Say (zsay)
import HoldEm as Poker




connect :: Config -> IO (Maybe Handle)
connect (Config srv port _ _ _) = do
    let handle = Network.connectTo srv (Network.PortNumber $ fromIntegral port)
    (fmap Just handle) `catchIOError` (const $ return Nothing)


run :: Config -> IO ()
run cfg@(Config _ _ chans nck mpswd) = Network.withSocketsDo $ do
    mhandle <- connect cfg
    case mhandle of
        Nothing -> putStrLn "No connection."
        Just handle -> do
            chan <- Conc.newChan
            login nck mpswd chan
            join' chans chan
            thdrecv <- Conc.forkIO (recv handle chan)
            thdinput <- Conc.forkIO (input chan)
            send cfg handle chan
            Conc.killThread thdrecv
            Conc.killThread thdinput


input :: Conc.Chan Event -> IO ()
input chan = do
    line <- getLine
    Conc.writeChan chan (RawMessage line)
    input chan


login :: String -> Maybe String -> Conc.Chan Event -> IO ()
login nck mpswd chan =
  do Conc.writeChan chan (Send $ nickMsg nck)
     Conc.writeChan chan (Send $ userMsg nck 0 "*" nck)
     case mpswd of 
        Nothing -> return ()
        Just pswd -> Conc.writeChan chan (Send $ identifyMsg pswd)


join' :: [String] -> Conc.Chan Event -> IO ()
join' chans chan = mapM_ (Conc.writeChan chan . Send . joinMsg) chans


recv :: Handle -> Conc.Chan Event -> IO ()
recv conn chan = do
    again <- (recvMsg conn chan) `catchIOError` (const $ return False)
    when again (recv conn chan)


recvMsg :: Handle -> Conc.Chan Event -> IO Bool 
recvMsg conn chan =
  do line <- hGetLine conn
     putStrLn line
     case (IRC.decode $ toBString line) of
       Nothing -> return ()
       Just msg -> Conc.writeChan chan (Recv msg)
     return True


send :: Config -> Handle -> Conc.Chan Event -> IO ()
send cfg conn chan = 
  do again <- (sendMsg cfg conn chan) `catchIOError` (const $ return False)
     when again (send cfg conn chan)


sendMsg :: Config -> Handle -> Conc.Chan Event -> IO Bool
sendMsg cfg conn chan =
  do event <- Conc.readChan chan
     case event of
       Send msg -> do let m = toString (IRC.encode msg)
                      putStrLn m
                      hPutStrLn conn m
                      return True
       Recv msg -> replyMsg cfg conn msg >> return True
       RawMessage msg -> hPutStrLn conn msg >> putStrLn msg >> return True


replyMsg :: Config -> Handle -> IRC.Message -> IO ()
replyMsg _ conn (IRC.Message Nothing cmd params)
  | cmd == "PING" = reply "PONG"
  | otherwise = return ()
  where reply x = hPutStrLn conn x >> putStrLn x
replyMsg (Config _ _ _ nck _) conn (IRC.Message (Just (IRC.NickName sender _ _)) cmd (recvr:msg))
  | cmd == "PRIVMSG" && prefixWith "zsay " (toString (head msg)) =
       reply $ privmsg (zsay . drop 5 . unwords . map toString $ msg)
  | cmd == "PRIVMSG" && prefixWith (nck ++ " best-hand ") (toString (head msg)) =
       reply $ privmsg (solveBestHand $ drop (length nck + length (" best-hand " :: String)) $ unwords $ map toString msg)
  | cmd == "PRIVMSG" = if
       | any (flip addressing msg') hal9000Prefixes -> hal9000
       | any (flip addressing msg') magic8Prefixes -> magic8ball
       | otherwise -> return ()
  | otherwise = return ()
  where
    addressing m r = prefixWith (nck ++ " " ++ m)  r ||
                     prefixWith (nck ++ ", " ++ m) r ||
                     prefixWith (nck ++ ": " ++ m) r
    msg' = toString (head msg)
    hal9000Prefixes = ["will you open ", "can you open ", "may you open "]
    magic8Prefixes = ["was ","were ","will ","do ","did ","does ","is ","are ","can ","have ",
                      "has "]
    reply x = hPutStrLn conn x >> putStrLn x
    asker = toString (if toString recvr == nck then sender else recvr)
    privmsg m = "PRIVMSG " ++  asker ++ " :" ++ m
    magic8ball = do answer <- anyElem ["yes", "no", "idk. ask zear", "sometimes",
                                       "correct", "probably", "pository", "negatory",
                                       "possibly. query the pcercuei. he would know.",
                                       "maybe...", "huh?", "sure", "nope", "yeah", "yup",
                                       "yes", "no"]
                    reply $ privmsg answer
    hal9000 = reply . privmsg $ "sorry, I can't do that, " ++ toString sender
replyMsg _ _ _ = return ()


anyElem :: [b] -> IO b
anyElem xs = do idx <- Random.randomRIO (0, length xs -1)
                return $ xs !! idx


solveBestHand :: String -> String
solveBestHand _ = "fold, human"


prefixWith :: String -> String -> Bool
prefixWith xs ys = and $ zipWith (==) xs ys


nickMsg :: String -> IRC.Message
nickMsg n = IRC.Message Nothing "NICK" [toBString n]


userMsg :: String -> Int -> String -> String -> IRC.Message
userMsg u m t n = IRC.Message Nothing "USER" (map toBString [u, show m, t, ' ':n])


identifyMsg :: String -> IRC.Message
identifyMsg p = IRC.Message Nothing "PRIVMSG" ["nickserv", ":identify", toBString p]


zmsg :: String -> String -> IRC.Message
zmsg target msg = IRC.Message Nothing "PRIVMSG" (map toBString [target, ' ':(zsay msg)])


pongMsg :: IRC.Message
pongMsg = IRC.Message Nothing "PONG" []


joinMsg :: String -> IRC.Message
joinMsg chan = IRC.Message Nothing "JOIN" [toBString chan]


toString :: BS.ByteString -> String
toString = map (toEnum . fromEnum) . BS.unpack


toBString :: String -> BS.ByteString
toBString = BS.pack . map (toEnum . fromEnum)
