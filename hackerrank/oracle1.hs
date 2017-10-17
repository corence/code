-- https://www.hackerrank.com/challenges/oracle1/copy-from/56419798

import Control.Lens.Lens
import Control.Monad
import qualified Data.Map as Map -- todo: replace with vector
import Data.Map(Map(..))
import Data.List
import Data.Maybe

{-
10 0 0 2 1
2
0 1 YES
3 4 NO
-}

{-
10 0 0 2 1
5
0 1 NO
1 2 NO
2 3 NO
3 4 YES
4 5 YES
-}

data Sector = Sector {
    contents :: [Int],
    enemies :: [Int]
}

data QueryResult = QueryResult {
    index1 :: Int,
    index2 :: Int,
    isMatch :: Bool
}

sortWith :: Ord b => (a -> b) -> [a] -> [a]
sortWith func = sortBy (\x y -> compare (func x) (func y))

main = do
    numBalls <- getLine <&> words <&> head <&> read :: IO Int
    numQueries <- getLine <&> read :: IO Int
    queryResults <- getQueryResults numQueries
    let colors = markColors queryResults (greyGrid numBalls)
    let trimmedColors = reduceColors queryResults colors
    let output = case majority (Map.elems trimmedColors) of
                      White -> colors & Map.toList & filter ((== White) . snd) & map fst & head & show
                      Black -> colors & Map.toList & filter ((== Black) . snd) & map fst & head & show
                      otherwise -> nextQuery numQueries
    putStrLn output

greyGrid :: Int -> Map Int Color
greyGrid numBalls = foldr ((flip Map.insert) Grey) Map.empty [0..(numBalls-1)]

-- note: we don't care who wins in a tie
-- note: in theory this should just need Eq not Ord, but i wanted an easy-to-write implementation
majority :: Ord a => [a] -> a
majority things = foldr countThing Map.empty things & Map.toList & sortWith (negate . snd) & head & fst
    where countThing a counts = Map.insertWith (+) a 1 counts

getQueryResults :: Int -> IO [QueryResult]
getQueryResults numQueries = replicateM numQueries getQuery
    where getQuery = getLine <&> words <&> arrayToQueryResult
          arrayToQueryResult [source, target, action] = QueryResult (read source) (read target) (actionToBool action)
          arrayToQueryResult _ = error "hey this query is pretty weird"
          actionToBool "YES" = True
          actionToBool "NO" = False
          actionToBool _ = error "wtf action is that"

markColors :: [QueryResult] -> Map Int Color -> Map Int Color
markColors queryResults colors
  = if updated
        then markColors queryResults newColors
        else colors
    where (updated, newColors) = foldr bleedColor colors queryResults
          bleedColor queryResult colors
            = let i1 = index1 queryResult
                  i2 = index2 queryResult
                  same = isMatch queryResult
                  (source, target) = case (Map.lookup i1 colors, Map.lookup i2 colors) of
                                       (Just Grey, Just Grey) -> (Nothing, Nothing)
                                       (Just Grey, Just c2) -> (Just (swapColor c2), Just i1)
                                       (Just c1, Just Grey) -> (Just (swapColor c1), Just i2)
                                       otherwise -> (Nothing, Nothing)
                  swapColor Black = White
                  swapColor White = Black
                  swapColor _ = error "cant swap that"
                  in case (source, target) of
                      (Nothing, Nothing) -> (False, colors)
                      (Just newColor, Just target) -> (True, Map.insert target newColor)

reduceColors :: [QueryResult] -> Map Int Color -> Map Int Color
reduceColors queryResults colors = foldr filterMismatchedPairs colors
    where filterMismatchedPairs queryResult colors
            = case (isMatch queryResult, Map.lookup (index1 queryResult) colors, Map.lookup (index2 queryResult) colors) of
                (False, Just Grey, Just Grey) -> colors & Map.delete (index1 queryResult) & Map.delete (index2 queryResult)
                otherwise -> colors

data Action = Swap | Stay deriving (Show, Eq)
data Color = Black | White | Grey deriving (Show, Eq, Ord)
queryResults :: Int -> IO [Color]
queryResults numQueries = swaps <&> colors White <&> (White :)
    where swaps = replicateM numQueries (getLine <&> words <&> (!! 2) <&> (== "YES") <&> (\result -> if result then Stay else Swap))
          colors _ [] = []
          colors White (Stay:rs) = White : colors White rs
          colors White (Swap:rs) = Black : colors Black rs
          colors Black (Stay:rs) = Black : colors Black rs
          colors Black (Swap:rs) = White : colors White rs

solve :: [Color] -> Int
solve colors
  = if isMostlyWhite colors
        then 0
        else index Black colors
  where isMostlyWhite colors = length (filter (== White) colors) * 2 >= length colors

nextQuery :: Int -> String
nextQuery n = show n ++ " " ++ show (n + 1)

index :: Eq a => a -> [a] -> Int
index _ [] = error "couldn't find it"
index a (x:xs)
  | a == x = 0
  | otherwise = 1 + index a xs
