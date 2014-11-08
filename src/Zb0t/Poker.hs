{-# LANGUAGE TemplateHaskell #-}
module Zb0t.Poker where


import           Data.Functor (void)
import           Control.Applicative hiding ((<|>))
import qualified Data.Map as Map
import qualified System.Random as Random
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.String as Parsec
import           Text.Parsec ((<|>))
import           Control.Lens
import qualified HoldEm as HoldEm


r2, r3, r4, r5, r6, r7, r8, r9, r10, jack, queen, king, ace :: Parsec.Parser HoldEm.Rank
r2 = Parsec.string "2" >> return HoldEm.R2
r3 = Parsec.string "3" >> return HoldEm.R3
r4 = Parsec.string "4" >> return HoldEm.R4
r5 = Parsec.string "5" >> return HoldEm.R5
r6 = Parsec.string "6" >> return HoldEm.R6
r7 = Parsec.string "7" >> return HoldEm.R7
r8 = Parsec.string "8" >> return HoldEm.R8
r9 = Parsec.string "9" >> return HoldEm.R9
r10 = Parsec.string "10" >> return HoldEm.R10
jack = Parsec.string "J" >> return HoldEm.Jack
queen = Parsec.string "Q" >> return HoldEm.Queen
king = Parsec.string "K" >> return HoldEm.King
ace = Parsec.string "A" >> return HoldEm.Ace


clubs, hearts, diamonds, spades :: Parsec.Parser HoldEm.Suit
clubs = Parsec.char 'c' >> return HoldEm.Clubs
hearts = Parsec.char 'h' >> return HoldEm.Hearts
diamonds = Parsec.char 'd' >> return HoldEm.Diamonds
spades = Parsec.char 's' >> return HoldEm.Spades


check, fold, raise :: Parsec.Parser Move
check = Parsec.string "check" >> return Check
fold = Parsec.string "fold" >> return FoldMove
raise = do
    Parsec.string "raise"
    void $ Parsec.many1 Parsec.space
    val <- Parsec.many1 Parsec.digit
    return (Raise (read val))


rank :: Parsec.Parser HoldEm.Rank
rank = r2 <|> r3 <|> r4 <|> r5 <|> r6 <|> r7 <|> r8 <|> r9 <|> r10 <|>
       jack <|> queen <|> king <|> ace


suit :: Parsec.Parser HoldEm.Suit
suit = clubs <|> hearts <|> diamonds <|> spades


card :: Parsec.Parser HoldEm.Card
card = HoldEm.Card <$> rank <*> suit


cards :: Parsec.Parser [HoldEm.Card]
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
    , _pHand :: HoldEm.PHand 
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
    Check -> g & over turn (+1)
    FoldMove -> g & over turn (+1)
    Raise value -> g & over turn (+1)
