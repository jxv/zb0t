module HoldEm
    ( Rank(..)
    , Suit(..)
    , Card(..)
    , Hand(..)
    , PHand
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
        deriving (Show, Enum, Ord, Bounded, Eq)


data Suit = C | H | D | S
        deriving (Show, Enum, Bounded, Eq)


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


data Table = Table
    { flop :: (Card, Card, Card)
    , turn :: Card 
    , river :: Card
    } deriving (Show, Eq)


instance Show Card where
    show (Card r s) = (show r) ++ "-" ++ (show s)


instance Ord Card where
    compare (Card a _) (Card v _) = compare a v


instance Ord Hand where
    compare (SFlush a) (SFlush b) = compare a b
    compare (SFlush _) _ = GT
    compare (Kind4 a b) (Kind4 v w) = compare2 (a,b) (v,w)
    compare (Kind4 _ _) _ = GT
    compare (FHouse a b) (FHouse v w) = compare2 (a,b) (v,w)
    compare (FHouse _ _) _ = GT
    compare (Flush a b c d e) (Flush v w x y z) = compare5 (a,b,c,d,e) (v,w,x,y,z)
    compare (Flush _ _ _ _ _) _ = GT
    compare (Straight a) (Straight v) = compare a v
    compare (Straight _) _ = GT
    compare (Kind3 a b c) (Kind3 v w x) = compare3 (a,b,c) (v,w,x)
    compare (Kind3 _ _ _) _ = GT
    compare (Pair2 a b c) (Pair2 v w x) = compare3 (a,b,c) (v,w,x)
    compare (Pair2 _ _ _) _ = GT
    compare (Pair1 a b c d) (Pair1 v w x y) = compare4 (a,b,c,d) (v,w,x,y)
    compare (Pair1 _ _ _ _) _ = GT
    compare (High a b c d e) (High v w x y z) = compare5 (a,b,c,d,e) (v,w,x,y,z)


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
            

bestHand :: PHand -> Table -> Hand
bestHand (a,b) (Table (c,d,e) f g) = Maybe.fromMaybe high tryFst
 where
    seven = [a,b,c,d,e,f,g]
    handsMay = [ sFlushMay, kind4May, fHouseMay, flushMay
               , straightMay, kind3May, pair2May, pair1May ]
    tryFst = foldr (<|>) Nothing (map ($ seven) handsMay)
    high = let (v:w:x:y:z:_) = revSort seven
           in High (rank v) (rank w) (rank x) (rank y) (rank z)


pair1May :: [Card] -> Maybe Hand
pair1May xs = do
    (h:hs) <- Safe.takeExactMay 4 (clusterByRank xs)
    (a:_) <- Safe.takeExactMay 2 h
    (b:c:d:_) <- Safe.takeExactMay 3 (List.concat hs)
    Just $ Pair1 (rank a) (rank b) (rank c) (rank d)


pair2May :: [Card] -> Maybe Hand
pair2May xs = do
    (h:i:j:_) <- Safe.takeExactMay 3 (clusterByRank xs)
    (a:_) <- Safe.takeExactMay 2 h
    (b:_) <- Safe.takeExactMay 2 i
    c <- Safe.headMay j
    Just $ Pair2 (rank a) (rank b) (rank c)


kind3May :: [Card] -> Maybe Hand
kind3May xs = do
    (h:hs) <- Safe.takeExactMay 3 (clusterByRank xs)
    (a:_) <- Safe.takeExactMay 3 h
    (b:c:_) <- Safe.takeExactMay 2 (List.concat hs)
    Just $ Kind3 (rank a) (rank b) (rank c)


straightMay :: [Card] -> Maybe Hand
straightMay xs = do
    let xs' = revSort $ List.nubBy (\a b -> rank a == rank b) xs
    (a:_) <- Safe.headMay $ filter (\h -> length h == 5 && revConsecutive (map rank h))
                                   (List.subsequences xs')
    Just $ Straight (rank a)


revConsecutive :: (Enum a, Eq a) => [a] -> Bool
revConsecutive as = case as of
    [] -> True;
    [a] -> True
    (a:b:as) -> b /= maxBound && a == succ b && revConsecutive (b:as) 


flushMay :: [Card] -> Maybe Hand
flushMay xs = do
    h <- Safe.headMay (clusterBySuit xs)
    (a:b:c:d:e:_) <- Safe.takeExactMay 5 (revSort h)
    Just $ Flush (rank a) (rank b) (rank c) (rank d) (rank e)


fHouseMay :: [Card] -> Maybe Hand
fHouseMay xs = do
    (h:i:_) <- Safe.takeExactMay 2 (clusterByRank xs)
    (a:_) <- Safe.takeExactMay 3 h
    (b:_) <- Safe.takeExactMay 2 i
    Just $ FHouse (rank a) (rank b)


kind4May :: [Card] -> Maybe Hand
kind4May xs = do
    (h:i:_) <- Safe.takeExactMay 2 (clusterByRank xs)
    (a:_) <- Safe.takeExactMay 4 h
    b <- Safe.headMay i
    Just $ Kind4 (rank a) (rank b)


sFlushMay :: [Card] -> Maybe Hand
sFlushMay xs = do
    xs' <- Safe.headMay (clusterBySuit xs)
    Straight r <- flushMay xs'
    Just $ SFlush r


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
