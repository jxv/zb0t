module HoldEm
    ( Rank(..)
    , Suit(..)
    , Card(..)
    , Hand(..)
    , PHand
    , HandSet
    , Table(..)
    , deck
    , bestHand
    , deal
    ) where

import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified System.Random as Random
import qualified Safe as Safe
import qualified Safe.Exact as Safe
import Control.Applicative
import Control.Monad

-- | Value of a card.
data Rank = R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 | Jack | Queen | King | Ace
        deriving (Enum, Ord, Bounded, Eq)

-- | Family of a card.
data Suit = Clubs | Hearts | Diamonds | Spades
        deriving (Enum, Bounded, Eq)

-- | Combined record for Rank and Suit.
data Card = Card { rank :: Rank, suit :: Suit }
        deriving (Eq)

-- | The value of a playable hand.
--   Use its instance of Ord for comparing better hands.
data Hand
  = High Rank Rank Rank Rank Rank
  | Pair1 Rank Rank Rank Rank
  | Pair2 Rank Rank Rank
  | Kind3 Rank Rank Rank
  | Straight Rank
  | Flush Rank Rank Rank Rank Rank
  | FHouse Rank Rank
  | Kind4 Rank Rank
  | SFlush Rank
  deriving (Show, Eq, Ord)

-- | A dealt hand to a player.
type PHand = (Card, Card)

-- | A set of 5 cards which can be played.
type HandSet = (Card, Card, Card, Card, Card)

-- | The cards on the board.
data Table = Table
  { flop :: (Card, Card, Card)
  , turn :: Card
  , river :: Card 
  } deriving (Show, Eq)

instance Show Rank where
  show r = case r of
    R2 -> "2"
    R3 -> "3"
    R4 -> "4"
    R5 -> "5"
    R6 -> "6"
    R7 -> "7"
    R8 -> "8"
    R9 -> "9"
    R10 -> "10"
    Jack -> "J"
    Queen -> "Q"
    King -> "K"
    Ace -> "A"

instance Show Suit where
  show s = case s of
    Clubs -> "c"
    Hearts -> "h"
    Diamonds -> "d"
    Spades -> "s"

instance Show Card where
  show (Card r s) = (show r) ++ (show s)

-- | The Ord instance of Card compares the ranks and ignores suits.
instance Ord Card where
    compare (Card a _) (Card v _) = compare a v

-- | All possible cards in a deck.
deck :: [Card]
deck = [Card r s | s <- [minBound..maxBound], r <- [minBound..maxBound]]

-- | Generates initial data for a game.
--   The number of players must be between 2 and 9 inclusively.
--   And, the length of the returning [PHand] will match the number of players.
deal :: Random.RandomGen g => g -> Int -> Either String (Table, [PHand])
deal rg numPlayers
  | numPlayers < 2 || numPlayers > 9 = Left "must be 2-9 players"
  | otherwise = Right $ let
      (a:b:c:d:e:cs) = map snd $ List.sort $ zip (Random.randoms rg :: [Int]) deck
      foldPHands xs = let (x:y:_,xs') = List.splitAt 2 xs in (x,y) : foldPHands xs'
      in (Table (a,b,c) d e, take numPlayers $ foldPHands cs)
          

-- | Finds the highest scoring hand from a player's hand and the table.
bestHand :: PHand -> Table -> (Hand, HandSet)
bestHand (a,b) (Table (c,d,e) f g) = Maybe.fromMaybe high tryFst
 where
    seven = [a,b,c,d,e,f,g]
    handsMay = [sFlushMay, kind4May, fHouseMay, flushMay, straightMay, kind3May, pair2May, pair1May]
    tryFst = foldr (<|>) Nothing (map ($ seven) handsMay)
    high = let
      (v:w:x:y:z:_) = revSort seven
      in (High (rank v) (rank w) (rank x) (rank y) (rank z), (v,w,x,y,z))

-- | Finds 'One Pair'
pair1May :: [Card] -> Maybe (Hand, HandSet)
pair1May xs = do
  h:hs <- Safe.takeExactMay 4 (clusterByRank xs)
  a:b:[] <- Safe.takeExactMay 2 h
  c:d:e:[] <- Safe.takeExactMay 3 (List.concat hs)
  Just (Pair1 (rank a) (rank c) (rank d) (rank e), (a,b,c,d,e))

-- | Finds 'Two Pair'
pair2May :: [Card] -> Maybe (Hand, HandSet)
pair2May xs = do
  h:i:j:[] <- Safe.takeExactMay 3 (clusterByRank xs)
  a:b:[] <- Safe.takeExactMay 2 h
  c:d:[] <- Safe.takeExactMay 2 i
  e <- Safe.headMay j
  Just (Pair2 (rank a) (rank c) (rank e), (a,b,c,d,e))

-- | Finds 'Three of a Kind'
kind3May :: [Card] -> Maybe (Hand, HandSet)
kind3May xs = do
  h:hs <- Safe.takeExactMay 3 (clusterByRank xs)
  a:b:c:[] <- Safe.takeExactMay 3 h
  d:e:[] <- Safe.takeExactMay 2 (List.concat hs)
  Just (Kind3 (rank a) (rank d) (rank e), (a,b,c,d,e))

-- | Finds 'Straight'
straightMay :: [Card] -> Maybe (Hand, HandSet)
straightMay xs = do
  h <- Safe.headMay $ filter
          (\h -> let rs = map rank h
                 in length h == 5 && (revConsecutive rs || rs == [R5,R4,R3,R2,Ace]))
          (possible xs)
  a:b:c:d:e:[] <- Safe.takeExactMay 5 h
  Just (Straight (rank a), (a,b,c,d,e))
  where
    cvtLow x = if map rank x == [Ace,R5,R4,R3,R2] then (tail x) ++ [head x] else x
    possible = revSort . map cvtLow . List.subsequences . revSort . List.nubBy (\a b -> rank a == rank b)

-- | Finds 'Flush'
flushMay :: [Card] -> Maybe (Hand, HandSet)
flushMay xs = do
  h <- Safe.headMay (clusterBySuit xs)
  a:b:c:d:e:[] <- Safe.takeExactMay 5 (revSort h)
  Just (Flush (rank a) (rank b) (rank c) (rank d) (rank e), (a,b,c,d,e))

-- | Finds 'Full House'
fHouseMay :: [Card] -> Maybe (Hand, HandSet)
fHouseMay xs = do
  h:i:[] <- Safe.takeExactMay 2 (clusterByRank xs)
  a:b:c:[] <- Safe.takeExactMay 3 h
  d:e:[] <- Safe.takeExactMay 2 i
  Just (FHouse (rank a) (rank d), (a,b,c,d,e))

-- | Finds 'Four of a Kind'
kind4May :: [Card] -> Maybe (Hand, HandSet)
kind4May xs = do
  h:i:[] <- Safe.takeExactMay 2 (clusterByRank xs)
  a:b:c:d:[] <- Safe.takeExactMay 4 h
  e <- Safe.headMay i
  Just (Kind4 (rank a) (rank e), (a,b,c,d,e))

-- | Finds 'Straight Flush'
sFlushMay :: [Card] -> Maybe (Hand, HandSet)
sFlushMay xs = do
  xs' <- Safe.headMay (clusterBySuit xs)
  (h,set) <- flushMay xs'
  case h of Straight r -> Just (SFlush r, set); _ -> Nothing

-- | Separates all cards by rank, then descendingly sorts them by length.
clusterByRank :: [Card] -> [[Card]]
clusterByRank
  = List.sortBy (\a b -> compare (length b) (length a))
  . List.groupBy (\x y -> rank x == rank y)
  . revSort

-- | Separates all cards by suit, then descendingly sorts them by length.
clusterBySuit :: [Card] -> [[Card]]
clusterBySuit
  = List.sortBy (\a b -> compare (length b) (length a))
  . List.groupBy (\x y -> suit x == suit y)
  . List.sortBy (\a b -> cmp (suit a) (suit b))
  where
    cmp a b = case a of
      Clubs    -> case b of Clubs -> EQ; Hearts -> LT; Diamonds -> LT; Spades -> LT
      Hearts   -> case b of Clubs -> GT; Hearts -> EQ; Diamonds -> LT; Spades -> LT
      Diamonds -> case b of Clubs -> GT; Hearts -> GT; Diamonds -> EQ; Spades -> LT
      Spades   -> case b of Clubs -> GT; Hearts -> GT; Diamonds -> GT; Spades -> EQ

-- | A descending sort.
revSort :: Ord a => [a] -> [a]
revSort = List.sortBy (flip compare)

-- | Returns true if all elements are in consecutive and descending order.
revConsecutive :: (Enum a, Eq a, Bounded a) => [a] -> Bool
revConsecutive as = case as of
  [] -> True;
  [a] -> True
  (a:b:as) -> b /= maxBound && a == succ b && revConsecutive (b:as) 

-- | Quick test suite based on
-- > http://en.wikipedia.org/wiki/Texas_hold_%27em#Sample_showdown
tests :: Bool
tests = let
  tbl = Table (Card R4 Clubs, Card King Spades, Card R4 Hearts) (Card R8 Spades) (Card R7 Spades)
  bob = (Card Ace Clubs, Card R4 Diamonds)
  carol = (Card Ace Spades, Card R9 Spades)
  ted = (Card King Hearts, Card King Diamonds)
  alice = (Card R5 Diamonds, Card R6 Diamonds)
  in Kind3 R4 Ace King == fst (bestHand bob   tbl) &&
     Flush Ace King R9 R8 R7 == fst (bestHand carol tbl) &&
     FHouse King R4 == fst (bestHand ted   tbl) &&
     Straight R8 == fst (bestHand alice tbl)
