{-# LANGUAGE TemplateHaskell #-}
module Zb0t.Poker where

import Control.Lens
import HoldEm hiding (turn)
import Control.Applicative hiding ((<|>))
import qualified Data.Map as Map
import qualified System.Random as Random
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.String as Parsec
import Text.Parsec ((<|>))
import Data.Functor (void)

r2, r3, r4, r5, r6, r7, r8, r9, r10, jack, queen, king, ace :: Parsec.Parser Rank
r2 = Parsec.string "2" >> return R2
r3 = Parsec.string "3" >> return R3
r4 = Parsec.string "4" >> return R4
r5 = Parsec.string "5" >> return R5
r6 = Parsec.string "6" >> return R6
r7 = Parsec.string "7" >> return R7
r8 = Parsec.string "8" >> return R8
r9 = Parsec.string "9" >> return R9
r10 = Parsec.string "10" >> return R10
jack = Parsec.string "J" >> return Jack
queen = Parsec.string "Q" >> return Queen
king = Parsec.string "K" >> return King
ace = Parsec.string "A" >> return Ace

clubs, hearts, diamonds, spades :: Parsec.Parser Suit
clubs = Parsec.char 'c' >> return Clubs
hearts = Parsec.char 'h' >> return Hearts
diamonds = Parsec.char 'd' >> return Diamonds
spades = Parsec.char 's' >> return Spades

check, fold, raise :: Parsec.Parser Move
check = Parsec.string "check" >> return Check
fold = Parsec.string "fold" >> return FoldMove
raise = do
    Parsec.string "raise"
    void $ Parsec.many1 Parsec.space
    val <- Parsec.many1 Parsec.digit
    return (Raise (read val))

rank' :: Parsec.Parser Rank
rank' = r2 <|> r3 <|> r4 <|> r5 <|> r6 <|> r7 <|> r8 <|> r9 <|> r10 <|>
       jack <|> queen <|> king <|> ace

suit' :: Parsec.Parser Suit
suit' = clubs <|> hearts <|> diamonds <|> spades

card :: Parsec.Parser Card
card = Card <$> rank' <*> suit'

cards :: Parsec.Parser [Card]
cards = Parsec.many $ do
    Parsec.many1 Parsec.space
    card

move :: Parsec.Parser Move
move = check <|> fold <|> raise

type Name = String

data Move
    = Check
    | FoldMove
    | Raise Integer
    deriving (Show, Eq)

data Chip
    = Dealer
    | SmallBlind
    | BigBlind
    deriving (Show, Eq)

data Player = Player
    { _points :: Integer
    , _pHand :: PHand 
    } deriving (Show, Eq)

data Pot = Pot
    { _contributors :: [Int]
    , _totalValue:: Integer
    } deriving (Show, Eq)

data Config = Config
    { _playerNames :: [String]
    } deriving (Show, Eq)

data Game = Game
    { _config :: Config
    , _lastChange :: Int
    , _turn :: Int
    , _ante :: Integer
    , _pots :: [Pot]
    , _playerOrder :: [Int]
    , _players :: Map.Map Int Player
    , _playerPlaying :: Map.Map Int Bool
    , _playerChips :: Map.Map Int Chip
    } deriving (Show, Eq)

makeLenses ''Player
makeLenses ''Pot
makeLenses ''Config
makeLenses ''Game

doMove :: Int -> Game -> Move -> Game
doMove n g m = case m of
    Check -> g & turn +~ 1
    FoldMove -> g & turn +~ 1
    Raise value -> g & turn +~ 1
