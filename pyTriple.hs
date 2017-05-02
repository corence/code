import Control.Monad
import Control.Applicative

{-
pyTriple :: (Num t, Ord t) => t -> Amb r (t, t, t)
pyTriple n = do a <- anIntegerBetween 1 n
                b <- anIntegerBetween (a + 1) n
                c <- anIntegerBetween (b + 1) n
                when (a*a + b*b /= c*c) empty
                return (a,b,c)
                -}

{-
main = show trippy >>= putStrLn

trippy :: Either r (t, t, t)
trippy = length $ allValues $ pyTriple 100

allValues = [1,2,3]

anIntegerBetween :: Int -> Int -> Int
anIntegerBetween x _ = x

pyTriple :: (Num t, Ord t) => t -> Either r (t, t, t)
pyTriple n = (anIntegerBetween 1 n) >>= (\a -> 
                (anIntegerBetween (a + 1) n) >>= (\b ->
                (anIntegerBetween (b + 1) n) >>= (\c ->
                    (when (a*a + b*b /= c*c) empty) >>
                    (return (a,b,c)))))
-}

main = pure (show numbers) >>= putStrLn
         >> pure (show experiment) >>= putStrLn

numbers :: [(Int, Int)]
numbers = [1,2,3,4] >>= (\x -> 
                [2,3,4,5,6] >>= (\y ->
                    case x * y of
                        8 -> return (x, y)
                        _ -> []))



numbers2 :: [(Int, Int)]
numbers2 = do
           x <- [1,2,3,4]
           y <- [2,3,4,5,6]
           case x * y of
               8 -> return (x, y)
               _ -> []

{-
numbers3 :: [(Int, Int)]
numbers3 = do
           x <- [1,2,3,4]
           y <- [2,3,4,5,6]
           guard (x * y == 8)
           -}


data CoinType = Fair | Biased deriving (Show)

data Coin = Head | Tail deriving (Eq,Show)

toss :: CoinType -> [Coin]
toss Fair   = [Head, Tail]
toss Biased = [Head, Head]

pick :: [CoinType]
pick = [Fair, Biased]

experiment :: [CoinType]
experiment = do
  coin   <- pick         -- Pick a coin at random
  result <- toss coin    -- Toss it, to get a result
  guard (result == Head) -- We only care about results that come up Heads
  return coin

ensure :: (a -> Bool) -> a -> Maybe a
ensure predicate value = if predicate value then Just value else Nothing
  
experiment3 :: [CoinType]
experiment3 = do
  coin   <- pick         -- Pick a coin at random
  result <- toss coin    -- Toss it, to get a result
  if result == Head then [coin] else [] -- We only care about results that come up Heads
  
experiment4 :: [CoinType]
experiment4 = do
  coin   <- pick         -- Pick a coin at random
  result <- toss coin    -- Toss it, to get a result
  [coin | result == Head] -- We only care about results that come up Heads
  
experiment2 =
  pick >>= (\coin ->        -- Pick a coin at random. Type of this (>>=) is: [CoinType] >>= (CoinType -> [CoinType])
      toss coin >>= (\result ->    -- Toss it, to get a result. Type of this (>>=) is: [Coin] >>= (Coin -> [Coin])
          guard (result == Head) >> -- guard is super weird. In this case it's of type: Bool -> [()]
                                    -- The magic happens when we apply (>>) here: it becomes an expression of [()] >> [Coin]
                                    -- For whatever reason, (>>) when applied to lists means: "repeat the second list n times, where n is the length of the first list"
                                    -- Examples:
                                    --      [1,2,3] >> [5] = [5,5,5]
                                    --      [(),(),()] >> [8,9] = [8,9,8,9,8,9]
                                    --      [()] >> xs = xs
                                    --      [] >> _ = []
          return coin)) -- return, in this case, is of type: Coin -> [Coin]
