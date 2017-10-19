
import Control.Monad
import qualified Data.Map as Map -- todo: replace with vector
import Data.Map(Map(..))
import Data.List
import Data.Maybe
import Debug.Trace

(&) = flip ($)
(<&>) = flip (<$>)

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

sortWith :: Ord b => (a -> b) -> [a] -> [a]
sortWith func = sortBy (\x y -> compare (func x) (func y))

data Color = White | Black | Idk | None deriving (Eq, Show, Ord)
data QueryResult = QueryResult Int Int Bool

main = do
    numBalls <- getLine <&> words <&> head <&> read :: IO Int
    numQueries <- getLine <&> read :: IO Int
    queryResults <- getQueryResults numQueries
    let colors = markColors numBalls queryResults
    let winner = count2 colors & sortWith (negate . snd) & head & fst
    let output = case winner of
                      White -> colors & findIndex (== White) & fromJust & show
                      Black -> colors & findIndex (== Black) & fromJust & show
                      otherwise -> nextQuery numQueries
    -- putStrLn $ show colors ++ " -- " ++ show (count2 colors) -- this line is just for debugging
    putStrLn output

markColors :: Int -> [QueryResult] -> [Color]
markColors numBalls queryResults
  = baseColors & spreadColorsRight queryResults -- & cancelPairs queryResults
    where baseColors = White : replicate (numBalls - 1) Idk

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
spreadColorsRight queryResults colors = foldl' applyQuery colors queryResults
    where applyQuery colors (QueryResult source target same)
            = let color1 = colors !! source
                  color2 = colors !! target
              in case (color1, color2) of
                     (Idk, _) -> colors
                     (_, Idk) -> setIndex (newColor color1 same) target colors
                     otherwise -> colors
          newColor oldColor True = oldColor
          newColor White False = Black
          newColor Black False = White
          newColor _ _ = error "ummmmm but what color"

cancelPairs :: [QueryResult] -> [Color] -> [Color]
cancelPairs queryResults colors = foldr maybeCancelPair colors queryResults
    where maybeCancelPair (QueryResult source target same) colors
            | same = colors
            | color1 == Idk && color2 == Idk = colors & setIndex None source & setIndex None target & cancelPairs queryResults
            | otherwise = colors
            where color1 = colors !! source
                  color2 = colors !! target

count :: Eq a => [a] -> [(a, Int)]
count = foldr increment []
    where increment element = setWith addTuple ((== element) . fst) (element, 1)
          addTuple (k1, v1) (k2, v2) = (k1, v1 + v2)

-- less efficient but so much easier to write. is there an interface for listmaps?
count2 :: (Ord a, Show a) => [a] -> [(a, Int)]
count2 = (Map.toList . foldr (\a -> Map.insertWith (+) a 1) Map.empty)

setIndex :: a -> Int -> [a] -> [a]
setIndex _ _ [] = error "hey that is too far"
setIndex v 0 (a:as) = v : as
setIndex v n (a:as) = a : setIndex v (n - 1) as

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
