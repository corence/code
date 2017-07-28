
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

-- filters duplicates from a sorted list
uniq :: Eq a => [a] -> [a]
uniq [] = []
uniq (x:[]) = [x]
uniq (x:y:zs)
  | x == y = uniq (x:zs)
  | otherwise = x : uniq (y:zs)
