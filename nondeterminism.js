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

function pick() {
    return ['Fair', 'Biased'];
}

function toss(coinType) {
    switch (coinType) {
        case 'Fair': return ['Head', 'Tail'];
        case 'Biased': return ['Head', 'Head'];
    }
}

Object.defineProperty(Array.prototype, 'bind_array', {
    enumerable: false,
    value: function (func) {
        const results = this.map(func);
        return [].concat.apply([], results);
    }
});

function experiment_haskellish() {
    return pick().bind_array(function (coin) {
        return toss(coin).bind_array(function (result) {
            if (result == 'Head') {
                return [coin];
            } else {
                return [];
            }
        });
    });
}

function experiment_jsish() {
    pick().
    return pick().bind_array(function (coin) {
        return toss(coin).bind_array(function (result) {
            if (result == 'Head') {
                return [coin];
            } else {
                return [];
            }
        });
    });
}

console.log(experiment_haskellish());
