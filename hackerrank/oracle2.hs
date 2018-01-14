
import Control.Monad
import Control.Applicative
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
                      otherwise -> nextQueryValues queryResults (0, 1) & \(source, target) -> show source ++ " " ++ show target
    -- putStrLn $ show colors ++ " -- " ++ show (count2 colors) -- this line is just for debugging
    putStrLn output

markColors :: Int -> [QueryResult] -> [Color]
markColors numBalls queryResults
  = baseColors & spreadColorsRight queryResults -- & cancelPairs queryResults
    where baseColors = White : replicate (numBalls - 1) Idk

ascendingPairs :: Ord a => [a] -> [(a, a)]
ascendingPairs values = liftA2 (,) values values & filter (\(a, b) -> a < b)

nextQueryValues :: [Color] -> [QueryResult] -> (Int, Int) -> (Int, Int)
--nextQueryValues colors queryResults (nextSource, nextTarget) = (foldl' (advanceFrom fst) nextSource queryResults, foldl' (advanceFrom snd) nextTarget queryResults)
nextQueryValues colors queryResults nextValues = foldl' advance nextValues queryResults
    where advance (nextSource, nextTarget) (QueryResult source target _)
            = (pairWithKnownQueries ++ twoIdkQueries ++ knownWithIdkQueries) & filter alreadyQueried & head & (\(source, _, target, _) -> (source, target)) -- we assume a nonzero list here due to the semantics of the problem (and if it's not nonzero then there's not much we can do)
          pairWithKnownQueries = allPossibleQueries & filter (\(source, sourceColor, target, targetColor) -> elem (sourceColor, targetColor) [(White, Idk), (Black, Idk), (Idk, White), (Idk, Black)]) & filter (\(source, _, target, _) -> numQueriesIncluding source queryResults + numQueriesIncluding target queryResults == 1)
          numQueriesIncluding index = filter (\(QueryResult source target _) -> source == index || target == index)
          allPossibleQueries = ascendingPairs [0..(length colors - 1)] & map (\(source, target) -> (source, colors !! source, target, colors !! target))
    -- suppose we know [White, White, Black, White, Idk, Idk, Idk, Idk, None, None]
    -- also, we know that the first pair of Idk are equal
    -- we could:
    --  - make a query between a known color and an idk. This gives us 1 guaranteed new info
    --  - make a query between two Idk. This gives us a 50/50 chance of 2 info
    --  - make a query between a known color, and an Idk that is known to be equal to another Idk. This gives us 2 guaranteed new info!
    -- suppose values are [White, White, White, White, White, White, White, White, White, White]
    -- 1) query 1 2 -> True
    --
    -- rules
    -- a) never query None, ever
    -- b) never query 2 known values
    -- c) never query two Idk if one of them has a query on it -- this will just make a mess and i don't think it will improve efficiency
    -- d) if two Idk are same, then always query them against a known color
    -- e) if two Idk are different, then never query them again (actually this can never happen because they will be Nones)
    -- so if i do this:
    --  - query a pair of Idk
    --  - if they are diff, great
    --  - if they are same, then query them against something known
    -- using this strategy we have 50% chance of 2 info with 1 request; and 50% chance of 2 info with 2 requests
    -- we also keep the number of known cells always odd

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
