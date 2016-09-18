module Zb0t.Parse.Config where

import Control.Lens
import Control.Applicative hiding ((<|>))

import Zb0t.Config (Config(..))
import Control.Monad.Fix (fix)
import Data.Functor (void)
import Data.ByteString (ByteString)
import Text.Parsec.ByteString (Parser)
import Text.Parsec
  ( (<|>)
  , string
  , parserFail
  , manyTill
  , anyChar
  , option
  , try
  , skipMany
  , parse
  , newline
  , eof
  , digit
  , many1
  , ParseError
  )

import qualified Data.ByteString as BS 
import qualified Data.ByteString.Char8 as BSC
import qualified Data.List as L

makeConfig :: [String] -> Either String Config
makeConfig args = case parseConfig args of
  Left _ -> Left "Bad args."
  Right cfg -> Right cfg

parseConfig :: [String] -> Either ParseError Config
parseConfig = parse config "" . (BSC.pack . L.concat . L.intersperse "\n")

config :: Parser Config
config = do
  s <- server
  p <- option 6667 port
  n <- nick
  w <- option Nothing password
  c <- option [] channels
  return $ Config s p c n w

server :: Parser ByteString
server = do
  flags ["-s","--server"]
  void newline
  BSC.pack <$> manyTill anyChar (void newline)

port :: Parser Int
port = do
  flags ["-p","--port"]
  void newline
  p <- many1 digit
  void newline
  return (read p)

nick :: Parser ByteString
nick = do
  flags ["-n","--nick","--nickname"]
  void newline
  BSC.pack <$> manyTill anyChar (void newline <|> eof)

password :: Parser (Maybe ByteString)
password = do
  flags ["-w","--pass","--passwd","--password"]
  void newline
  Just . BSC.pack <$> manyTill anyChar (void newline <|> eof)

channels :: Parser [ByteString]
channels = do
  flags ["-c","--channel","--channels"]
  void newline
  chans <- fix $ \loop -> do
    c <- manyTill anyChar (void newline <|> eof) 
    (eof *> return [c]) <|> (loop >>= \cs -> return (c : cs))
  return (map BSC.pack chans)

flags :: [String] -> Parser ()
flags = void . foldr (\n ns -> ns <|> try (string n)) (parserFail "no flag elements") 
