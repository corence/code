
data Suit = Clubs | Diamonds | Spades | Hearts deriving (Eq, Ord, Enum, Show, Bounded)
data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten
          | Jack | Queen | King | Ace deriving (Eq, Ord, Enum, Show, Bounded)
data Card = Card Suit Rank deriving (Eq, Ord, Show, Bounded)

all_values :: (Bounded a, Enum a) => [a]
all_values = [minBound ..]

all_cards = zipWith Card all_values all_values

main :: IO ()
main = do
    (Card Clubs Queen) <- all_cards
    return ()

black_jack :: Card
black_jack = do
    (Card suit rank) <- all_cards
    guard (suit == Clubs)
    guard (rank == Jack)
