{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Zb0t.Core
    ( run
    ) where

import Control.Lens
import Text.Parsec
import Control.Applicative hiding ((<|>), join)
import Zb0t.Types
import qualified Network as Net
import qualified Network.IRC as IRC
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.List as L
import qualified System.Random as Random
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Combinator as Parsec
import qualified Text.Parsec.String as Parsec
import qualified Safe as Safe
import qualified Zb0t.Config as Cfg
import qualified Zb0t.Say as Say
import qualified Zb0t.Poker as Poker
import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.Chan (newChan, dupChan, writeChan, readChan, Chan)
import Control.Concurrent.Async (async, wait)
import Control.Monad (when, void)
import Control.Monad.Trans (liftIO)
import Data.ByteString (ByteString)
import GHC.IO.Handle (Handle)
import System.IO (hPutStrLn,hGetLine)
import System.IO.Error (catchIOError)
import Zb0t.Config (Config(..))

data CommandTag
    = PING
    | PONG
    | PRIVMSG
    deriving (Show, Read, Eq, Enum, Bounded)

type Parser = Parsec.ParsecT ByteString () IO

connect :: Config -> IO (Maybe Handle)
connect (Config srv port _ _ _) = do
    let handle = Net.connectTo (BSC.unpack srv) (Net.PortNumber $ fromIntegral port)
    (fmap Just handle) `catchIOError` (const $ return Nothing)

run :: Config -> IO ()
run cfg@(Config _ _ chans nck mpswd) = Net.withSocketsDo $ do
    mhandle <- connect cfg
    case mhandle of
        Nothing -> putStrLn "No connection."
        Just handle -> do
            chan <- newChan
            login nck mpswd chan
            joinChannel chans chan
            thdrecv <- forkIO (recv handle chan)
            thdinput <- forkIO (input chan)
            mapM_ (forkIO . flip salutate chan) chans
            thds <- mapM (forkIO . flip shameless chan) chans
            send cfg handle chan
            mapM_ killThread thds
            killThread thdrecv
            killThread thdinput

salutate :: ByteString -> Chan Event -> IO ()
salutate channel chan = do
    intro <- anyElem introductions
    let evt = Send $ IRC.Message Nothing "PRIVMSG" [channel, intro]
    writeChan chan evt
    
   
introductions :: [ByteString] 
introductions = ["hi","hello","hey","hola","sup"]

shameless :: ByteString -> Chan Event -> IO ()
shameless channel chan = do
    r <- Random.randomRIO (300 * 12, 300 * 24)
    threadDelay (r * 1000000)
    interruption <- anyElem interruptions
    let evt = Send $ IRC.Message Nothing "PRIVMSG" [channel, interruption]
    -- writeChan chan evt
    shameless channel chan
   

interruptions :: [ByteString]
interruptions =
    ["hueueueueue","lol","curvature","zbrt","woop woop woop","zqck"] ++ introductions

input :: Chan Event -> IO ()
input chan = do
    line <- getLine
    writeChan chan (RawMessage line)
    input chan

login :: ByteString -> Maybe ByteString -> Chan Event -> IO ()
login nck mpswd chan = do
    writeChan chan (Send $ IRC.nick nck)
    writeChan chan (Send $ IRC.user nck "0" "*" nck)
    case mpswd of 
        Nothing -> return ()
        Just pswd -> writeChan chan (Send $ identify pswd)

joinChannel :: [ByteString] -> Chan Event -> IO ()
joinChannel chans chan = mapM_ (writeChan chan . Send . IRC.joinChan) chans

recv :: Handle -> Chan Event -> IO ()
recv conn chan = do
    again <- (recvMsg conn chan) `catchIOError` (const $ return False)
    when again (recv conn chan)

recvMsg :: Handle -> Chan Event -> IO Bool 
recvMsg conn chan = do
    line <- hGetLine conn
    putStrLn line
    case IRC.decode (BSC.pack line) of
        Nothing -> return ()
        Just msg -> writeChan chan (Recv msg)
    return True

send :: Config -> Handle -> Chan Event -> IO ()
send cfg conn chan = do
    again <- (sendMsg cfg conn chan) `catchIOError` (const $ return False)
    when again (send cfg conn chan)

sendMsg :: Config -> Handle -> Chan Event -> IO Bool
sendMsg cfg conn chan = do
    event <- readChan chan
    case event of
        RawMessage msg -> do
            hPutStrLn conn msg
            putStrLn msg
        Send msg -> do
            let m = BSC.unpack (IRC.encode msg)
            putStrLn m
            hPutStrLn conn m
        Recv msg -> do
            mMsg <- replyMsg cfg msg
            case mMsg of
                Nothing -> return ()
                Just event -> writeChan chan event
    return True

replyMsg :: Config -> IRC.Message -> IO (Maybe Event)
replyMsg _ msg@(IRC.Message Nothing command params) = case Safe.readMay (BSC.unpack command) of
    Just PING -> return $ Just (Send (IRC.pong ""))
    _ -> return Nothing
replyMsg cfg (IRC.Message (Just (IRC.NickName sender _ _)) command params) = do
    let nick = cfg^.Cfg.nick
    let receiver = head params
    let asker = if receiver == nick then sender else receiver
    let body = BS.intercalate " " (tail params)
    let respond str = return
                    . Just
                    . Send 
                    $ IRC.Message Nothing "PRIVMSG" [asker, BS.empty, str]
    case Safe.readMay (BSC.unpack command) of
        Just PRIVMSG -> do
            result <- Parsec.runPT (response nick sender) () "" body
            either (const $ return Nothing) respond result
        _ -> return Nothing
replyMsg _ _ = return Nothing

response :: ByteString -> ByteString -> Parser ByteString
response self sender = foldr1 (<|>)
    [ try $ zsayP >> space >> say
    , do void $ bstrings introductions
         void $ many1 space
         void $ string (BSC.unpack self)
         liftIO (anyElem introductions)
    , addressResponse self sender
    ]

addressResponse :: ByteString -> ByteString -> Parser ByteString
addressResponse self sender = do
    void (string (BSC.unpack self))
    void addressSuffix
    foldr1 (<|>)
        [ try (string "say") >> space >> say
        , try anagramP >> many1 space >> anagram ((<= 5) . length)
        , bstrings introductions >> liftIO (anyElem introductions)
        , hal9000Prefix >> many1 space >> return (hal9000Response sender)
        , magic8Prefix >> many1 space >> liftIO magic8Response
        ]

anagram :: (String -> Bool) -> Parser ByteString
anagram isWord = do
    word <- BSC.pack <$> manyTill Parsec.alphaNum (void space <|> eof)
    if BS.length word >= 10
       then parserFail ""
       else return . BS.intercalate " " . anagrams isWord $ word

say :: Parser ByteString
say = BSC.pack . Say.zsay <$> manyTill anyChar eof

addressSuffix :: Parser String
addressSuffix = strings [" ",", ",": "]

zsayP :: Parser String
zsayP = string "zsay"

anagramP :: Parser String
anagramP = string "anagram"

strings :: [String] -> Parser String
strings = choice . map (try . string)

bstrings :: [ByteString] -> Parser ByteString
bstrings = fmap BSC.pack . strings . map BSC.unpack

hal9000Prefix :: Parser ByteString
hal9000Prefix = bstrings
    ["will you open", "can you open", "may you open","open the"]

magic8Prefix :: Parser ByteString
magic8Prefix = bstrings
    ["was","were ","will","do","did","does","is","are","can","have","has","would","could"]

magic8Response :: IO ByteString
magic8Response = anyElem
    ["yes","no","idk. ask zrkw","sometimes","correct","probably","pository","negatory"
    ,"possibly. query the pcercuei. he would know.","maybe...", "huh?", "sure", "nope"
    ,"yeah", "yup","yes", "no"]

hal9000Response :: ByteString -> ByteString
hal9000Response n = BS.concat ["sorry, ", n,". I can't do that."]

anyElem :: [b] -> IO b
anyElem xs = do
    idx <- Random.randomRIO (0, length xs -1)
    return $ xs !! idx

solveBestHand :: ByteString -> ByteString
solveBestHand _ = "fold, human"

prefixWith :: String -> String -> Bool
prefixWith xs ys = and $ zipWith (==) xs ys

identify :: ByteString -> IRC.Message
identify pswd = IRC.Message Nothing "PRIVMSG" ["nickserv", ":identify", pswd]

zmsg :: String -> String -> IRC.Message
zmsg target msg = IRC.Message Nothing "PRIVMSG" (map BSC.pack [target, ' ':(Say.zsay msg)])

anagrams :: (String -> Bool) -> ByteString -> [ByteString]
anagrams isWord = map BSC.pack . filter isWord . L.nub . L.permutations . BSC.unpack
