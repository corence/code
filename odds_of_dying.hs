
import qualified Data.Map as Map
import Data.Map(Map(..))
import Data.List

-- given cells .....
-- and 2 splits
-- possible splits are
-- ...**
-- ..*.*
-- .*..*
-- *...*
-- ..**.
-- .*.*.
-- *..*.
-- .**..
-- *.*..
-- **...
-- == 10

sortWith :: Ord o => (a -> o) -> [a] -> [a]
sortWith func = sortBy (\a b -> compare (func a) (func b))

factorial :: Integral a => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- nPr = n! / (n - r)!
nPr :: Integral a => a -> a -> a
nPr n r = (factorial n) `div` (factorial $ n - r)

-- nCr = n! / r! (n - r)!
nCr :: Integral a => a -> a -> a
nCr n r = nPr n r `div` factorial r

type Possibility = (Integer, Double) -- (population, chance of this happening)

whatHappensNext :: Possibility -> [Possibility]
whatHappensNext (numDudes, odds) = foldr (\numSplits result -> numSplitResults numSplits : result) [] [0..numDudes]
    where numSplitResults numSplits = (numSplits * 2, odds * fromIntegral (nCr numDudes numSplits) * (0.75 ^^ numSplits) * (0.25 ^^ (numDudes - numSplits)))

-- merge all possibilities that share the same number of dudes
merge :: [Possibility] -> [Possibility]
merge possibilities = Map.toList $ foldl (\r possibility -> Map.insertWith (+) (fst possibility) (snd possibility) r) Map.empty possibilities

world :: [[Possibility]]
world = let results = [(2, 1)] : map (concatMap whatHappensNext) results
        in map merge $ results

main = mapM_ (putStrLn . show) (map (sortWith (negate . snd)) $ take 10 world)
