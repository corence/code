
import qualified Data.Set as Set
import Data.Set(Set(..))
import Data.List
import qualified Heap as Heap
import Heap(Heap(..))

(&) = flip ($)

type Index = Int
type Value = Int
type Depth = Int
type Pattern = [Value]

data Estimate = Estimate Depth Pattern
data Attempt = Attempt Pattern Int Int -- matched positions, unmatched positions

depth [] = 1
depth (0:pattern) = patternLength * depth pattern
depth (_:pattern) = depth pattern

estimate :: Pattern -> Estimate
estimate pattern = Estimate (depth pattern) pattern

patternLength = 4
initialPattern = take patternLength $ repeat 0

makePatterns :: Int -> Pattern -> Index -> [Pattern]
makePatterns patternLength pattern index = map insertValue [0..(patternLength - 1)]
    where insertValue value = take (index - 1) pattern ++ [value] ++ drop index pattern -- probably there's a library function for this

-- ....
-- ...1, ...2, ...3
-- ..21, ..31, ...3

-- given a set of possibilities
-- randomly choose one to expand
-- to expand it:
--  - remove it from the list
--  - put all expansions back into the list
-- keep doing this until we expand a full-length response (depth = 1)

-- given a bunch of possibilities
-- randomly pick one (if that's too hard then pick one with the highest depth)
-- if it is fully-expanded: attempt it
-- if not: remove it, expand it, put its children in
type Status = (Pattern, Collection, [Attempt])

type Collection = Heap Int Estimate

action :: Status -> Status
action status@(answer, collection, attempts) =
    case Heap.query collection of
        Nothing -> status
        Just (Estimate 1 pattern) -> makeAttempt pattern status
        Just (Estimate _ pattern) -> (answer, expandInCollection pattern collection, attempts)
        where expandInCollection pattern collection = foldr add (Heap.remove_head collection) (expand attempts pattern)
              add pattern = Heap.add (negate $ depth pattern, Estimate (depth pattern) pattern)

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
makeAttempt pattern status@(answer, collection, attempts) = (answer, collection, attempt : attempts)
    where attempt = Attempt pattern matchy unmatchy
          (matchy, unmatchy) = score answer pattern

expand :: [Attempt] -> Pattern -> [Pattern]
expand attempts pattern
    = [0..(patternLength - 1)] -- we now have a [Index]
    & map (makePatterns patternLength pattern) -- we now have a [[Pattern]]
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

main = pure ()

-- assume pattern is RRO
-- RGR 1 1
-- ROV (invalid guess)
-- RVR (invalid guess!) matchy too high
-- RRG (invalid guess!) score between RRG and RGR is: (1, 2) but that's a total of 3 which is too high
-- PRP (invalid guess!) matchy is too low

-- OOR 0 2
-- OGA (invalid guess) matchy too high
