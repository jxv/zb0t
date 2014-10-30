{-# LANGUAGE LambdaCase #-}
module Zb0t.HoldEm
    ( Rank(..)
    , Suit(..)
    , Card(..)
    , Hand(..)
    , PHand
    , HandSet
    , Table(..)
    , bestHand
    , deal
    ) where


import           Control.Applicative
import           Control.Monad
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified System.Random as Random
import qualified Safe       as Safe
import qualified Safe.Exact as Safe


data Rank = R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 | J | Q | K | A
        deriving (Enum, Ord, Bounded, Eq)


data Suit = C | H | D | S
        deriving (Enum, Bounded, Eq)


data Card = Card { rank :: Rank, suit :: Suit }
        deriving (Eq)


data Hand
    = SFlush    Rank
    | Kind4     Rank Rank
    | FHouse    Rank Rank
    | Flush     Rank Rank Rank Rank Rank
    | Straight  Rank
    | Kind3     Rank Rank Rank
    | Pair2     Rank Rank Rank
    | Pair1     Rank Rank Rank Rank
    | High      Rank Rank Rank Rank Rank
    deriving (Show, Eq)


type PHand = (Card, Card)
type HandSet = (Card, Card, Card, Card, Card)


data Table = Table
    { flop :: (Card, Card, Card)
    , turn :: Card 
    , river :: Card
    } deriving (Show, Eq)


instance Show Rank where
    show r = case r of
        R2 -> "2"; R3 -> "3"; R4 -> "4"; R5 -> "5"; R6 -> "6"; R7 -> "7"; R8 -> "8"
        R9 -> "9"; R10 -> "10"; J -> "J"; Q -> "Q"; K -> "K"; A -> "A"

instance Show Suit where
    show s = case s of C -> "c"; H -> "h"; D -> "d"; S -> "s"


instance Show Card where
    show (Card r s) = (show r) ++ (show s)


instance Ord Card where
    compare (Card a _) (Card v _) = compare a v


instance Ord Hand where
    compare (SFlush a) = \case
        SFlush b -> compare a b
        _ -> GT
    compare (Kind4 a b) = \case
        Kind4 v w -> compare2 (a,b) (v,w)
        SFlush{} -> LT
        _ -> GT
    compare (FHouse a b) = \case
        FHouse v w -> compare2 (a,b) (v,w)
        SFlush{} -> LT
        Kind4{} -> LT
        _ -> GT
    compare (Flush a b c d e) = \case
        Flush v w x y z -> compare5 (a,b,c,d,e) (v,w,x,y,z)
        SFlush{} -> LT
        Kind4{} -> LT
        FHouse{} -> LT
        _ -> GT
    compare (Straight a) = \case
        Straight v -> compare a v
        SFlush{} -> LT
        Kind4{} -> LT
        FHouse{} -> LT
        Flush{} -> LT
        _ -> GT
    compare (Kind3 a b c) = \case
        Kind3 v w x -> compare3 (a,b,c) (v,w,x)
        Pair2{} -> GT
        Pair1{} -> GT
        High{} -> GT
        _ -> LT
    compare (Pair2 a b c) = \case
        Pair2 v w x -> compare3 (a,b,c) (v,w,x)
        Pair1{} -> GT
        High{} -> GT
        _ -> LT
    compare (Pair1 a b c d) = \case
        Pair1 v w x y -> compare4 (a,b,c,d) (v,w,x,y)
        High{} -> GT
        _ -> LT
    compare (High a b c d e) = \case
        High v w x y z -> compare5 (a,b,c,d,e) (v,w,x,y,z)
        _ -> LT


compare2 :: (Rank, Rank) -> (Rank, Rank) -> Ordering
compare2 (a,b) (v,w) = case compare a v of EQ -> compare b w; o -> o


compare3 :: (Rank, Rank, Rank) -> (Rank, Rank, Rank) -> Ordering
compare3 (a,b,c) (v,w,x) = case compare a v of EQ -> compare2 (b,c) (w,x); o -> o


compare4 :: (Rank, Rank, Rank, Rank) -> (Rank, Rank, Rank, Rank) -> Ordering
compare4 (a,b,c,d) (v,w,x,y) = case compare a v of EQ -> compare3 (b,c,d) (w,x,y); o -> o


compare5 :: (Rank, Rank, Rank, Rank, Rank) -> (Rank, Rank, Rank, Rank, Rank) -> Ordering
compare5 (a,b,c,d,e) (v,w,x,y,z) =
    case compare a v of EQ -> compare4 (b,c,d,e) (w,x,y,z); o -> o


deck :: [Card]
deck = [Card r s | s <- [minBound..maxBound], r <- [minBound..maxBound]]


deal :: Random.RandomGen g => g -> Int -> Either String (Table, [PHand])
deal rg numPlayers
  | numPlayers < 2 || numPlayers > 9 = Left "must be 2-9 players"
  | otherwise = Right $
        let (a:b:c:d:e:cs) = map snd $ List.sort $ zip (Random.randoms rg :: [Int]) deck
            foldPHands xs = let (x:y:_,xs') = List.splitAt 2 xs in (x,y) : foldPHands xs'
        in (Table (a,b,c) d e, take numPlayers $ foldPHands cs)
            

bestHand :: PHand -> Table -> (Hand, HandSet)
bestHand (a,b) (Table (c,d,e) f g) = Maybe.fromMaybe high tryFst
 where
    seven = [a,b,c,d,e,f,g]
    handsMay = [ sFlushMay, kind4May, fHouseMay, flushMay
               , straightMay, kind3May, pair2May, pair1May ]
    tryFst = foldr (<|>) Nothing (map ($ seven) handsMay)
    high = let (v:w:x:y:z:_) = revSort seven
           in (High (rank v) (rank w) (rank x) (rank y) (rank z), (v,w,x,y,z))


pair1May :: [Card] -> Maybe (Hand, HandSet)
pair1May xs = do
    h:hs <- Safe.takeExactMay 4 (clusterByRank xs)
    a:b:[] <- Safe.takeExactMay 2 h
    c:d:e:[] <- Safe.takeExactMay 3 (List.concat hs)
    Just (Pair1 (rank a) (rank c) (rank d) (rank e), (a,b,c,d,e))


pair2May :: [Card] -> Maybe (Hand, HandSet)
pair2May xs = do
    h:i:j:[] <- Safe.takeExactMay 3 (clusterByRank xs)
    a:b:[] <- Safe.takeExactMay 2 h
    c:d:[] <- Safe.takeExactMay 2 i
    e <- Safe.headMay j
    Just (Pair2 (rank a) (rank c) (rank e), (a,b,c,d,e))


kind3May :: [Card] -> Maybe (Hand, HandSet)
kind3May xs = do
    h:hs <- Safe.takeExactMay 3 (clusterByRank xs)
    a:b:c:[] <- Safe.takeExactMay 3 h
    d:e:[] <- Safe.takeExactMay 2 (List.concat hs)
    Just (Kind3 (rank a) (rank d) (rank e), (a,b,c,d,e))


straightMay :: [Card] -> Maybe (Hand, HandSet)
straightMay xs = do
    h <- Safe.headMay $ filter
            (\h -> let rs = map rank h
                   in length h == 5 && (revConsecutive rs || rs == [R5,R4,R3,R2,A]))
            (possible xs)
    a:b:c:d:e:[] <- Safe.takeExactMay 5 h
    Just (Straight (rank a), (a,b,c,d,e))
 where
    cvtLow x = if map rank x == [A,R5,R4,R3,R2] then (tail x) ++ [head x] else x
    possible = revSort
             . map cvtLow
             . List.subsequences
             . revSort
             . List.nubBy (\a b -> rank a == rank b)


revConsecutive :: (Enum a, Eq a, Bounded a) => [a] -> Bool
revConsecutive as = case as of
    [] -> True;
    [a] -> True
    (a:b:as) -> b /= maxBound && a == succ b && revConsecutive (b:as) 


flushMay :: [Card] -> Maybe (Hand, HandSet)
flushMay xs = do
    h <- Safe.headMay (clusterBySuit xs)
    a:b:c:d:e:[] <- Safe.takeExactMay 5 (revSort h)
    Just (Flush (rank a) (rank b) (rank c) (rank d) (rank e), (a,b,c,d,e))


fHouseMay :: [Card] -> Maybe (Hand, HandSet)
fHouseMay xs = do
    h:i:[] <- Safe.takeExactMay 2 (clusterByRank xs)
    a:b:c:[] <- Safe.takeExactMay 3 h
    d:e:[] <- Safe.takeExactMay 2 i
    Just (FHouse (rank a) (rank d), (a,b,c,d,e))


kind4May :: [Card] -> Maybe (Hand, HandSet)
kind4May xs = do
    h:i:[] <- Safe.takeExactMay 2 (clusterByRank xs)
    a:b:c:d:[] <- Safe.takeExactMay 4 h
    e <- Safe.headMay i
    Just (Kind4 (rank a) (rank e), (a,b,c,d,e))


sFlushMay :: [Card] -> Maybe (Hand, HandSet)
sFlushMay xs = do
    xs' <- Safe.headMay (clusterBySuit xs)
    (h,set) <- flushMay xs'
    case h of Straight r -> Just (SFlush r, set); _ -> Nothing


clusterByRank :: [Card] -> [[Card]]
clusterByRank = List.sortBy (\a b -> compare (length b) (length a))
              . List.groupBy (\x y -> rank x == rank y)
              . revSort


clusterBySuit :: [Card] -> [[Card]]
clusterBySuit = List.sortBy (\a b -> compare (length b) (length a))
              . List.groupBy (\x y -> suit x == suit y)
              . List.sortBy (\a b -> cmp (suit a) (suit b))
 where cmp a b = case a of C -> case b of C -> EQ; H -> LT; D -> LT; S -> LT
                           H -> case b of C -> GT; H -> EQ; D -> LT; S -> LT
                           D -> case b of C -> GT; H -> GT; D -> EQ; S -> LT
                           S -> case b of C -> GT; H -> GT; D -> GT; S -> EQ


revSort :: Ord a => [a] -> [a]
revSort = List.sortBy (flip compare)

-- http://en.wikipedia.org/wiki/Texas_hold_%27em#Sample_showdown

tests :: Bool
tests =
    let tbl   = Table (Card R4 C, Card K S, Card R4 H) (Card R8 S) (Card R7 S)
        bob   = (Card A C, Card R4 D)
        carol = (Card A S, Card R9 S)
        ted   = (Card K H, Card K D)
        alice = (Card R5 D, Card R6 D)
    in Kind3 R4 A K       == fst (bestHand bob   tbl) &&
       Flush A K R9 R8 R7 == fst (bestHand carol tbl) &&
       FHouse K R4        == fst (bestHand ted   tbl) &&
       Straight R8        == fst (bestHand alice tbl)


