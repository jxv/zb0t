module HoldEm where


data Rank = R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 | J | Q | K | A
        deriving (Show, Enum, Ord, Bounded, Eq)


data Suit = C | H | D | S
        deriving (Show, Enum, Bounded, Eq)


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
