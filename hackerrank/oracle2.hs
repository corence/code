
import Control.Lens.Lens
import Control.Monad
import qualified Data.Map as Map -- todo: replace with vector
import Data.Map(Map(..))
import Data.List
import Data.Maybe

sortWith :: Ord b => (a -> b) -> [a] -> [a]
sortWith func = sortBy (\x y -> compare (func x) (func y))

index :: Eq a => (a -> Bool) -> [a] -> Int
index _ [] = error "not found bro"
index predicate (a:as)
  = if predicate a then 0 else 1 + index predicate as

data Color = White | Black | Idk | None deriving (Eq, Show)
data QueryResult = QueryResult Int Int Bool

main = do
    numBalls <- getLine <&> words <&> head <&> read :: IO Int
    numQueries <- getLine <&> read :: IO Int
    queryResults <- getQueryResults numQueries
    let baseColors = White : replicate (numBalls - 1) Idk
    let colors2 = spreadColorsRight queryResults baseColors
    let colors3 = cancelPairs queryResults colors2
    let counts = count colors3
    let winner = sortWith (negate . snd) counts & head & fst
    let output = case winner of
                      White -> colors2 & index (== White) & show
                      Black -> colors2 & index (== Black) & show
                      otherwise -> nextQuery numQueries
    putStrLn output

nextQuery :: Int -> String
nextQuery n = show n ++ " " ++ show (n + 1)

getQueryResults :: Int -> IO [QueryResult]
getQueryResults numQueries = replicateM numQueries getQueryResult

getQueryResult = getLine <&> words <&> arrayToQueryResult
    where arrayToQueryResult [source, target, action] = QueryResult (read source) (read target) (actionToBool action)
          arrayToQueryResult _ = error "hey this query is pretty weird"
          actionToBool "YES" = True
          actionToBool "NO" = False
          actionToBool _ = error "wtf action is that"

spreadColorsRight :: [QueryResult] -> [Color] -> [Color]
spreadColorsRight queryResults colors = foldr applyQuery colors queryResults
    where applyQuery (QueryResult source target same) colors
            = let color1 = colors !! source
                  color2 = colors !! target
              in case (color1, color2) of
                     (Idk, _) -> colors
                     (_, Idk) -> set (newColor color1 same) target colors
                     otherwise -> colors
          newColor oldColor True = oldColor
          newColor White False = Black
          newColor Black False = White
          newColor _ _ = error "ummmmm but what color"

-- TODO: rewrite this as a foldr
cancelPairs :: [QueryResult] -> [Color] -> [Color]
cancelPairs [] colors = colors
cancelPairs ((QueryResult _ _ True) : queryResults) colors = cancelPairs queryResults colors
cancelPairs ((QueryResult source target False) : queryResults) colors
  = let color1 = colors !! source
        color2 = colors !! target
    in case (color1, color2) of
           (Idk, Idk) -> colors & set None source & set None target & cancelPairs queryResults
           otherwise -> cancelPairs queryResults colors

count :: Eq a => [a] -> [(a, Int)]
count = foldr increment []
    where increment element = setWith addTuple ((== element) . fst) (element, 1)
          addTuple (k1, v1) (k2, v2) = (k1, v1 + v2)

set :: a -> Int -> [a] -> [a]
set _ _ [] = error "hey that is too far"
set a 0 as = a : as
set a n (_:as) = set a (n - 1) as

-- modify function (apply it with the value to whatever element passes the predicate),
-- predicate (to see if this is one of the things we want to modify),
-- default value (if the predicate doesn't match anything),
-- list to modify
setWith :: Eq a => (a -> a -> a) -> (a -> Bool) -> a -> [a] -> [a]
setWith func predicate value [] = [value]
setWith func predicate value (a:as)
  = if predicate a
        then func a value : as
        else setWith func predicate value as
