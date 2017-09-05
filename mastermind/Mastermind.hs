
import qualified Data.Set as Set
import Data.Set(Set(..))
import Data.List
import qualified Heap as Heap
import Heap(Heap(..))
import Data.Char

(&) = flip ($)

type Index = Int
type Value = Char
type Depth = Int
type Pattern = [Value]

data Estimate = Estimate Depth Pattern deriving Show
data Attempt = Attempt Pattern Int Int deriving Show -- matched positions, unmatched positions

numZeroes :: Pattern -> Int
numZeroes = length . filter (== '.')

numMatchy :: Attempt -> Int
numMatchy (Attempt _ matchy _) = matchy

estimate :: Pattern -> Estimate
estimate pattern = Estimate (numZeroes pattern) pattern

initialPattern :: Int -> Pattern
initialPattern patternLength = take patternLength $ repeat '.'

makePatterns :: Int -> Pattern -> Index -> [Pattern]
makePatterns numValues pattern index = map insertValue ['a'..(chr ((ord 'a') + numValues - 1))]
    where insertValue value = take (index - 1) pattern ++ [value] ++ drop index pattern -- probably there's a library function for this

-- ....
-- ...1, ...2, ...3
-- ..21, ..31, ...3

-- given a set of possibilities
-- randomly choose one to expand
-- to expand it:
--  - remove it from the list
--  - put all expansions back into the list
-- keep doing this until we expand a full-length response (numZeroes = 0)

-- given a bunch of possibilities
-- randomly pick one (if that's too hard then pick one with the highest numZeroes)
-- if it is fully-expanded: attempt it
-- if not: remove it, expand it, put its children in
type Status = (Int, Pattern, Collection, [Attempt])

type Collection = Heap Int Estimate

reduce :: Status -> Status
reduce status@(numValues, answer, collection, attempts) =
    if (filter isSolution attempts & length & (> 0))
        then status
        else reduce $ action status
        where isSolution attempt = numMatchy attempt == length answer

action :: Status -> Status
action status@(numValues, answer, collection, attempts) =
    case Heap.query collection of
        Nothing -> status
        Just (Estimate 0 pattern) -> makeAttempt pattern status
        Just (Estimate _ pattern) -> (numValues, answer, expandInCollection pattern collection, attempts)
        where expandInCollection pattern collection = foldr addToCollection (Heap.remove_head collection) (expand numValues attempts pattern)

addToCollection pattern = Heap.add (numZeroes pattern, Estimate (numZeroes pattern) pattern)
{-
action :: Status -> Status
action status@(collection, attempts)
  = case Heap.query collection of
      Nothing -> status
      Just (depth, pattern) -> if depth == 1
                           then makeAttempt pattern status
                           else expandAndReplace estimate collection
        where expandAndReplace estimate collection = foldr Heap.add (Heap.remove_head collection) (newEstimatesFrom estimate)
              newEstimatesFrom estimate = expand attempts estimate
-}

makeAttempt :: Pattern -> Status -> Status
makeAttempt pattern status@(numValues, answer, collection, attempts) = (numValues, answer, collection, attempt : attempts)
    where attempt = Attempt pattern matchy unmatchy
          (matchy, unmatchy) = score answer pattern

expand :: Int -> [Attempt] -> Pattern -> [Pattern]
expand numValues attempts pattern
    = [0..(numValues - 1)] -- we now have a [Index]
    & map (makePatterns numValues pattern) -- we now have a [[Pattern]]
    & concat -- we now have a [Pattern]
    & filter (\pattern -> all (satisfiesAttempt pattern) attempts) -- foreach pattern: it should satisfy all attempts

-- fail if:
-- the total score is higher than the recorded total
-- the matchy score is lower OR higher than the recorded matchy
satisfiesAttempt :: Pattern -> Attempt -> Bool
satisfiesAttempt pattern (Attempt aPattern aMatchy aUnmatchy)
    | matchy /= aMatchy = False
    | (matchy + unmatchy) /= (aMatchy + aUnmatchy) = False
    | otherwise = True
    where (matchy, unmatchy) = score pattern aPattern

score pattern1 pattern2 = (matchyScore pattern1 pattern2, unmatchyScore pattern1 pattern2)

matchyScore [] [] = 0
matchyScore _ [] = error "can't score unequal length patterns"
matchyScore [] _ = error "can't score patterns of unequal length"
matchyScore (x:xs) (y:ys)
  | x == y = 1 + matchyScore xs ys
  | otherwise = matchyScore xs ys

unmatchyScore (x:xs) ys
  | elem x ys = 1 + unmatchyScore xs (delete x ys)
  | otherwise = unmatchyScore xs (tail ys)

main = do
    putStrLn $ show $ initialPattern 3
    putStrLn $ show $ reduce (3, "abc", addToCollection (initialPattern 3) Heap.empty, [])

-- assume pattern is RRO
-- RGR 1 1
-- ROV (invalid guess)
-- RVR (invalid guess!) matchy too high
-- RRG (invalid guess!) score between RRG and RGR is: (1, 2) but that's a total of 3 which is too high
-- PRP (invalid guess!) matchy is too low

-- OOR 0 2
-- OGA (invalid guess) matchy too high
