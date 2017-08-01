
import Data.List
import Control.Monad
import qualified Data.Set as Set
import Data.Set(Set(..))

main = do
    numCases <- read <$> getLine :: IO Int
    sequence $ Data.List.replicate numCases doTestCase

doTestCase :: IO ()
doTestCase = do
    string <- getLine
    index <- (subtract 1) <$> read <$> getLine
    let char = getFromSubsequences index string
    putChar char >> putChar '\n'

getFromSubsequences :: Int -> String -> Char
--getFromSubsequences index string = (join . map head . group . sort . subsequences) string !! index
getFromSubsequences index string = (join . Set.toList . Set.fromList . subsequences) string !! index
    --(join (Set.toList (Set.fromList (subsequences string)))) !! index
    --(join . map head . group . sort . subsequences) string !! index

-- given a string, return an array that is all of the distinct substrings, sorted
snowball :: String -> [String]
snowball = uniq . sort . subsequences

-- given a string, return a sorted set that is all of the distinct substrings
snowset :: String -> Set String
snowset = Set.fromList . subsequences

-- given a string, return a sorted set that is all of the distinct substrings
snowset2 :: String -> [String]
snowset2 = Set.toList . Set.fromList . subsequences

-- filters duplicates from a sorted list
uniq :: Eq a => [a] -> [a]
uniq [] = []
uniq (x:[]) = [x]
uniq (x:y:zs)
  | x == y = uniq (x:zs)
  | otherwise = x : uniq (y:zs)

something :: IO Int
something = do
    index1 <- (subtract 1) <$> read <$> getLine
    index2 <- (subtract 1) <$> read <$> getLine
    pure (index1 + index2)

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip (<$>)
    
getIndex :: IO Int
getIndex = getLine <&> read <&> subtract 1
