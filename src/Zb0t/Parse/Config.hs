module Zb0t.Parse.Config where

import Zb0t.Config (Config(..))
import Control.Monad.Fix (fix)
import Control.Applicative hiding ((<|>))
import Data.Functor (void)
import Text.Parsec.String (Parser)
import Text.Parsec ((<|>), string, parserFail, manyTill, anyChar, option, try,
                    skipMany, parse, newline, eof, digit, many1, ParseError)
import qualified Data.ByteString as BS
import qualified Data.List as L
import Control.Lens
import Data.Default
import Zb0t.Types

makeConfig :: [String] -> Either String Config
makeConfig args = case parseConfig args of
    Left _ -> Left "Bad args."
    Right cfg -> Right cfg

parseConfig :: [String] -> Either ParseError Config
parseConfig = parse config "" . (L.concat . L.intersperse "\n")

config :: Parser Config
config = do
    s <- server
    p <- option 6667 port
    n <- nick
    w <- option Nothing password
    c <- option [] channels
    return $ Config s p c n w

server :: Parser String
server = do
    flags ["-s","--server"]
    void newline
    manyTill anyChar (void newline)

port :: Parser Int
port = do
    flags ["-p","--port"]
    void newline
    p <- many1 digit
    void newline
    return (read p)

nick :: Parser String
nick = do
    flags ["-n","--nick","--nickname"]
    void newline
    manyTill anyChar (void newline <|> eof)

password :: Parser (Maybe String)
password = do
    flags ["-w","--pass","--passwd","--password"]
    void newline
    Just <$> manyTill anyChar (void newline <|> eof)

channels :: Parser [String]
channels = do
    flags ["-c","--channel","--channels"]
    void newline
    fix $ \loop -> do
        c <- manyTill anyChar (void newline <|> eof) 
        (eof *> return [c]) <|> (do cs <- loop
                                    return (c : cs))

flags :: [String] -> Parser ()
flags = void . foldr (\n ns -> ns <|> try (string n)) (parserFail "no flag elements") 
