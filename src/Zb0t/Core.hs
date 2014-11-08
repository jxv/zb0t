{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Zb0t.Core
    ( run
    ) where

import           Data.Functor (void)
import           Control.Monad (when)
import           Control.Monad.Trans (liftIO)
import           Control.Applicative hiding ((<|>), join)
import           GHC.IO.Handle (Handle)
import           System.IO (hPutStrLn,hGetLine)
import           System.IO.Error (catchIOError)
import qualified Control.Concurrent as Conc
import qualified Control.Concurrent.Chan as Conc
import qualified Control.Concurrent.Async as Conc
import qualified Network as Net
import qualified Network.IRC as IRC
import qualified Data.ByteString as BS hiding (pack, unpack)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL hiding (pack, unpack)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.List as L
import qualified System.Random as Random
import           Text.Parsec ((<|>), string, parserFail, manyTill, anyChar, option, try,
                              skipMany, parse, newline, eof, digit, many1, ParseError,
                              choice, space, skipMany, skipMany1)
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Combinator as Parsec
import qualified Text.Parsec.String as Parsec
import qualified Safe as Safe


import Zb0t.Types
import Zb0t.Say (zsay)
import qualified Zb0t.Poker as Poker


data CommandTag
    = PING
    | PONG
    | PRIVMSG
    deriving (Show, Read, Eq, Enum, Bounded)


type Parser = Parsec.ParsecT String () IO


connect :: Config -> IO (Maybe Handle)
connect (Config srv port _ _ _) = do
    let handle = Net.connectTo srv (Net.PortNumber $ fromIntegral port)
    (fmap Just handle) `catchIOError` (const $ return Nothing)


run :: Config -> IO ()
run cfg@(Config _ _ chans nck mpswd) = Net.withSocketsDo $ do
    mhandle <- connect cfg
    case mhandle of
        Nothing -> putStrLn "No connection."
        Just handle -> do
            chan <- Conc.newChan
            login nck mpswd chan
            joinChannel chans chan
            thdrecv <- Conc.forkIO (recv handle chan)
            thdinput <- Conc.forkIO (input chan)
            mapM_ (Conc.forkIO . flip salutate chan) chans
            thds <- mapM (Conc.forkIO . flip shameless chan) chans
            send cfg handle chan
            mapM_ Conc.killThread thds
            Conc.killThread thdrecv
            Conc.killThread thdinput


salutate :: String -> Conc.Chan Event -> IO ()
salutate channel chan = do
    intro <- anyElem introductions
    let evt = Send $ IRC.Message Nothing "PRIVMSG" [BS.pack channel, intro]
    Conc.writeChan chan evt
    
   
introductions :: [BS.ByteString] 
introductions = ["hi","hello","hey","hola"]


shameless :: String -> Conc.Chan Event -> IO ()
shameless channel chan = do
    r <- Random.randomRIO (300 * 12, 300 * 24)
    Conc.threadDelay (r * 1000000)
    interruption <- anyElem interruptions
    let evt = Send $ IRC.Message Nothing "PRIVMSG" [BS.pack channel, interruption]
    Conc.writeChan chan evt
    shameless channel chan
   

interruptions :: [BS.ByteString]
interruptions =
    ["hueueueueue","lol","curvature","zbrt","woop woop woop","zqck"] ++ introductions


input :: Conc.Chan Event -> IO ()
input chan = do
    line <- getLine
    Conc.writeChan chan (RawMessage line)
    input chan


login :: String -> Maybe String -> Conc.Chan Event -> IO ()
login nck mpswd chan = do
    Conc.writeChan chan (Send $ nickMsg nck)
    Conc.writeChan chan (Send $ userMsg nck 0 "*" nck)
    case mpswd of 
        Nothing -> return ()
        Just pswd -> Conc.writeChan chan (Send $ identifyMsg pswd)


joinChannel :: [String] -> Conc.Chan Event -> IO ()
joinChannel chans chan = mapM_ (Conc.writeChan chan . Send . joinMsg) chans


recv :: Handle -> Conc.Chan Event -> IO ()
recv conn chan = do
    again <- (recvMsg conn chan) `catchIOError` (const $ return False)
    when again (recv conn chan)


recvMsg :: Handle -> Conc.Chan Event -> IO Bool 
recvMsg conn chan = do
    line <- hGetLine conn
    putStrLn line
    case IRC.decode (BS.pack line) of
        Nothing -> return ()
        Just msg -> Conc.writeChan chan (Recv msg)
    return True


send :: Config -> Handle -> Conc.Chan Event -> IO ()
send cfg conn chan = do
    again <- (sendMsg cfg conn chan) `catchIOError` (const $ return False)
    when again (send cfg conn chan)


sendMsg :: Config -> Handle -> Conc.Chan Event -> IO Bool
sendMsg cfg conn chan = do
    event <- Conc.readChan chan
    case event of
        RawMessage msg -> do
            hPutStrLn conn msg
            putStrLn msg
        Send msg -> do
            let m = BS.unpack (IRC.encode msg)
            putStrLn m
            hPutStrLn conn m
        Recv msg -> do
            mMsg <- replyMsg cfg msg
            case mMsg of
                Nothing -> return ()
                Just event -> Conc.writeChan chan event
    return True

replyMsg :: Config -> IRC.Message -> IO (Maybe Event)
replyMsg _ msg@(IRC.Message Nothing command params) = case Safe.readMay (BS.unpack command) of
    Just PING -> return $ Just (Send pongMsg)
    _ -> return Nothing
replyMsg  Config{..} (IRC.Message (Just (IRC.NickName sender' _ _)) command params) = do
    let sender = BS.unpack sender'
    let receiver = BS.unpack (head params)
    let asker = if receiver == cfgNick then sender else receiver
    let body = unwords $ map BS.unpack (tail params)
    let respond str = return
                    . Just
                    . Send 
                    $ IRC.Message Nothing "PRIVMSG" [BS.pack asker, BS.empty, BS.pack str]
    case Safe.readMay (BS.unpack command) of
        Just PRIVMSG -> do
            result <- Parsec.runPT (response cfgNick sender) () "" body
            either (const $ return Nothing) respond result
        _ -> return Nothing
replyMsg _ _ = return Nothing


response :: String -> String -> Parser String
response self sender = foldr1 (<|>)
    [ try $ zsayP >> space >> say
    , do strings (map BS.unpack introductions)
         void $ many1 space
         void $ string self
         liftIO (anyElem $ map BS.unpack introductions)
    , addressResponse self sender
    ]


addressResponse :: String -> String -> Parser String
addressResponse self sender = do
    void (string self)
    void addressSuffix
    foldr1 (<|>)
        [ try (string "say") >> space >> say
        , try anagramP >> many1 space >> anagram ((<= 5) . length)
        , strings (map BS.unpack introductions) >> liftIO (anyElem $ map BS.unpack introductions)
        , hal9000Prefix >> many1 space >> return (hal9000Response sender)
        , magic8Prefix >> many1 space >> liftIO magic8Response
        ]


anagram :: (String -> Bool) -> Parser String
anagram isWord = do
    word <- manyTill Parsec.alphaNum (void space <|> eof)
    if length word >= 10
       then parserFail ""
       else return . unwords . anagrams isWord $ word


say :: Parser String
say = do
    saying <- manyTill anyChar eof
    return (zsay saying)


addressSuffix :: Parser String
addressSuffix = strings [" ",", ",": "]


zsayP :: Parser String
zsayP = string "zsay"


anagramP :: Parser String
anagramP = string "anagram"


strings :: [String] -> Parser String
strings = choice . map (try . string)


hal9000Prefix :: Parser String
hal9000Prefix = strings
    ["will you open", "can you open", "may you open","open the"]


magic8Prefix :: Parser String
magic8Prefix = strings
    ["was","were ","will","do","did","does","is","are","can","have","has","would","could"]


magic8Response :: IO String
magic8Response = anyElem
    ["yes","no","idk. ask zrkw","sometimes","correct","probably","pository","negatory"
    ,"possibly. query the pcercuei. he would know.","maybe...", "huh?", "sure", "nope"
    ,"yeah", "yup","yes", "no"]


hal9000Response :: String -> String
hal9000Response n = "sorry, I can't do that, " ++ n ++ "."


anyElem :: [b] -> IO b
anyElem xs = do
    idx <- Random.randomRIO (0, length xs -1)
    return $ xs !! idx


solveBestHand :: String -> String
solveBestHand _ = "fold, human"


prefixWith :: String -> String -> Bool
prefixWith xs ys = and $ zipWith (==) xs ys


nickMsg :: String -> IRC.Message
nickMsg n = IRC.Message Nothing "NICK" [BS.pack n]


userMsg :: String -> Int -> String -> String -> IRC.Message
userMsg u m t n = IRC.Message Nothing "USER" (map BS.pack [u, show m, t, ' ':n])


identifyMsg :: String -> IRC.Message
identifyMsg p = IRC.Message Nothing "PRIVMSG" ["nickserv", ":identify", BS.pack p]


zmsg :: String -> String -> IRC.Message
zmsg target msg = IRC.Message Nothing "PRIVMSG" (map BS.pack [target, ' ':(zsay msg)])


pongMsg :: IRC.Message
pongMsg = IRC.Message Nothing "PONG" []


joinMsg :: String -> IRC.Message
joinMsg chan = IRC.Message Nothing "JOIN" [BS.pack chan]


anagrams :: (String -> Bool) -> String -> [String]
anagrams isWord = filter isWord . L.nub . L.permutations
