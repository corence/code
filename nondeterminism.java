/*
   Haskell implementation that we're copying:
   
data CoinType = Fair | Biased deriving (Show)

data Coin = Head | Tail deriving (Eq,Show)

toss :: CoinType -> [Coin]
toss Fair   = [Head, Tail]
toss Biased = [Head, Head]

pick :: [CoinType]
pick = [Fair, Biased]


experiment = do
  coin   <- pick         -- Pick a coin at random
  result <- toss coin    -- Toss it, to get a result
  guard (result == Head) -- We only care about results that come up Heads
  return coin     
*/

public class nondeterminism {
    public enum CoinType {
        Fair,
        Biased
    }

    public enum Coin {
        Head,
        Tail
    }

    public static List<CoinType> pick() {
        return CoinType.values();
    }

    public static List<Coin> flip(CoinType coinType) {
        switch(coinType) {
            case Fair: return new ArrayList<>(Head, Tail);
            case Biased: return new ArrayList<>(Head, Head);
        }
    }

    public static List<CoinType> experiment() {
        
    }
}
