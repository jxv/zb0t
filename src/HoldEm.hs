{-# LANGUAGE MultiWayIf #-}
module HoldEm where


import           Control.Applicative
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified System.Random as Random


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
    tryFst = (mayKind3 seven) <|>
             (mayPair2 seven) <|>
             (mayPair1 seven)
    high = let (v:w:x:y:z:_) = map rank $ List.sortBy (flip compare) seven
           in High v w x y z


mayPair1 :: [Card] -> Maybe Hand
mayPair1 xs
  | length xs > 7 || length xs < 5 = Nothing
  | otherwise =
        let sorted = List.sortBy (\a b -> compare (length b) (length a))
                                 (List.group (List.sort xs))
            h = head sorted
            i = head $ drop 1 sorted
            (a:b:c:_) = map rank (List.concat $ drop 1 sorted)
        in if length h == 2 && length i /= 2
              then Just $ Pair1 (rank $ head h) a b c 
              else Nothing


mayPair2 :: [Card] -> Maybe Hand
mayPair2 xs
  | length xs > 7 || length xs < 5 = Nothing
  | otherwise =
        let sorted = List.sortBy (\a b -> compare (length b) (length a))
                                 (List.group (List.sort xs))
            h = head sorted
            i = head $ drop 1 sorted
            j = head $ drop 2 sorted
            (a:_) = map rank (List.concat $ drop 2 sorted)
        in if length h == 2 && length i == 2 && length j /= 2
              then Just $ Pair2 (rank $ head h) (rank $ head i) a
              else Nothing


mayKind3 :: [Card] -> Maybe Hand
mayKind3 xs
  | length xs > 7 || length xs < 5 = Nothing
  | otherwise =
        let sorted = List.sortBy (\a b -> compare (length b) (length a))
                                 (List.group (List.sort xs))
            h = head sorted
            i = head $ drop 1 sorted
            (a:b:_) = map rank (List.concat $ drop 1 sorted)
        in if length h == 3 && length i /= 3
              then Just $ Pair1 (rank $ head h) a b
              else Nothing
