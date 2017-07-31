
{-# LANGUAGE NoImplicitPrelude #-} 
import Data.Text
import qualified Data.Text.IO as TextIO
import qualified Data.Text.Read as TextRead
import Data.List
import Control.Monad
import qualified Data.Set as Set
import Data.Set(Set(..))
import BasicPrelude

main = do
    numCases <- TextRead.read <$> TextIO.getLine :: IO Int
    sequence $ Data.List.replicate numCases doTestCase

doTestCase :: IO ()
doTestCase = do
    string <- TextIO.getLine
    index <- (subtract 1) <$> TextRead.read <$> TextIO.getLine
    let char = getFromSubsequences index string
    putChar char >> putChar '\n'

getFromSubsequences :: Int -> Text -> Char
--getFromSubsequences index string = (join . map head . group . sort . subsequences) string !! index
getFromSubsequences index string = (join . Set.toList . Set.fromList . subsequences) string !! index
    --(join (Set.toList (Set.fromList (subsequences string)))) !! index
    --(join . map head . group . sort . subsequences) string !! index

-- given a string, return an array that is all of the distinct substrings, sorted
snowball :: Text -> [Text]
snowball = uniq . sort . subsequences

-- given a string, return a sorted set that is all of the distinct substrings
snowset :: Text -> Set Text
snowset = Set.fromList . subsequences

-- given a string, return a sorted set that is all of the distinct substrings
snowset2 :: Text -> [Text]
snowset2 = Set.toList . Set.fromList . subsequences

-- filters duplicates from a sorted list
uniq :: Eq a => [a] -> [a]
uniq [] = []
uniq (x:[]) = [x]
uniq (x:y:zs)
  | x == y = uniq (x:zs)
  | otherwise = x : uniq (y:zs)
