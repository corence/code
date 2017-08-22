
import qualified Data.Map as Map
import Data.Map(Map(..))

type Possibility = (Int, Double) -- (population, chance of this happening)

whatHappensNext :: Possibility -> [Possibility]
whatHappensNext (0, odds) = [(0, odds)]
whatHappensNext (dudes, odds) = resultsOfDeath ++ resultsOfLife
    where resultsOfLife = map incrementDudes (whatHappensNext ((dudes - 1), odds * 0.75))
          resultsOfDeath = whatHappensNext ((dudes - 1), odds * 0.25)
          incrementDudes (dudes, odds) = (dudes + 2, odds)

-- merge all possibilities that share the same number of dudes
merge :: [Possibility] -> [Possibility]
merge possibilities = Map.toList $ foldl (\r possibility -> Map.insertWith (+) (fst possibility) (snd possibility) r) Map.empty possibilities

world :: [[Possibility]]
world = let results = [(2, 1)] : map (concatMap whatHappensNext) results
        in map merge $ results

main = mapM_ (putStrLn . show) (take 2 world)
