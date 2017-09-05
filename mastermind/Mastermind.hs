
import qualified Data.Set as Set
import Data.Set(Set(..))
import Data.List
import Data.Char

import Debug.Trace

(&) = flip ($)

ntrace _ = id

type Index = Int
type Value = Char
type Depth = Int
type Pattern = [Value]

data Attempt = Attempt Pattern Int Int deriving Show -- matched positions, unmatched positions

numZeroes :: Pattern -> Int
numZeroes = length . filter (== '.')

numMatchy :: Attempt -> Int
numMatchy (Attempt _ matchy _) = matchy

initialPattern :: Int -> Pattern
initialPattern patternLength = take patternLength $ repeat '.'

makePatterns :: Int -> Pattern -> Index -> [Pattern]
makePatterns numValues pattern index
  | pattern !! index /= '.' = [] -- if it's not a dot, then throw this away -- we have better expansions on the way
  | otherwise = map insertValue ['a'..(chr ((ord 'a') + numValues - 1))]
    where insertValue value = take index pattern ++ [value] ++ drop (index + 1) pattern -- probably there's a library function for this

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
type Status = (Int, Pattern, Candidates, [Attempt])

type Candidates = [Pattern]

reduce :: Status -> Status
reduce status@(numValues, answer, candidates, attempts)
  | not (null solutions) = status
  | null candidates = status
  | otherwise = trace ("attempts " ++ show (length attempts) ++ ", candidates " ++ show (length candidates)) $ reduce $ action status
        where isSolution attempt = numMatchy attempt == length answer
              solutions = filter isSolution attempts

action :: Status -> Status
action status@(_, _, [], _) = status
action status@(numValues, answer, (candidate:candidates), attempts) =
    if numZeroes candidate == 0
        then makeAttempt candidate (statusWith candidates)
        else statusWith $ expandInCollection candidate candidates
        where expandInCollection candidate candidates = foldr addToCollection candidates (expand numValues attempts candidate)
              statusWith newCollection = (numValues, answer, newCollection, attempts)

addToCollection = (:)

makeAttempt :: Pattern -> Status -> Status
makeAttempt pattern status@(numValues, answer, candidates, attempts)
  | all (satisfiesAttempt pattern) attempts = (numValues, answer, candidates, attempts ++ [attempt])
  | otherwise = status
    where attempt = Attempt pattern matchy unmatchy
          (matchy, unmatchy) = score answer pattern

expand :: Int -> [Attempt] -> Pattern -> [Pattern]
expand numValues attempts pattern
    = [0..((length pattern) - 1)] -- we now have a [Index]
    & map (makePatterns numValues pattern) -- we now have a [[Pattern]]
    & concat -- we now have a [Pattern]
    & filter (\pattern -> all (satisfiesAttempt pattern) attempts) -- foreach pattern: it should satisfy all attempts

-- fail if:
-- the total score is higher than the recorded total
-- the matchy score is lower OR higher than the recorded matchy
satisfiesAttempt :: Pattern -> Attempt -> Bool
satisfiesAttempt pattern (Attempt aPattern aMatchy aUnmatchy)
    | matchy > aMatchy = False -- if there are more matches between these patterns than the attempt's score, we've gone too far
    | unmatchy > aUnmatchy = False
    | matchy + numZeroes pattern < aMatchy = False -- if we assume all our dots are matches, will we have enough matches to match?
    | unmatchy + numZeroes pattern < aUnmatchy = False
    | otherwise = True
    where (matchy, unmatchy) = score pattern aPattern

-- generates a score by comparing 2 patterns.
-- Note that unlike the classic boardgame, our second score is how many total matching colours there are (ignoring position)
-- It is therefore always >= the first value.
score :: Pattern -> Pattern -> (Int, Int)
score pattern1 pattern2 = (matchyScore pattern1 pattern2, unmatchyScore pattern1 pattern2)

matchyScore [] [] = 0
matchyScore _ [] = error "can't score unequal length patterns"
matchyScore [] _ = error "can't score patterns of unequal length"
matchyScore (x:xs) (y:ys)
  | x == y = 1 + matchyScore xs ys
  | otherwise = matchyScore xs ys

unmatchyScore [] [] = 0
unmatchyScore (x:xs) ys
  | elem x ys = 1 + unmatchyScore xs (delete x ys)
  | otherwise = unmatchyScore xs (tail ys)

initialStatus :: Int -> Pattern -> Status
initialStatus numValues answer = (numValues, answer, addToCollection (initialPattern (length answer)) [], [])

main = putStrLn $ displayStatus $ reduce $ initialStatus 4 "abaddaab"

displayStatus :: Status -> String
displayStatus status@(numValues, answer, candidates, attempts)
    = "Toward answer " ++ answer ++ " -> \n"
    ++ (concat $ map displayAttempt attempts)
    ++ if null candidates then "Dead end.\n" else ""
    where displayAttempt (Attempt pattern matchy unmatchy) = "Attempt " ++ pattern ++ "! Score " ++ show matchy ++ ":" ++ show unmatchy ++ "\n"

-- assume pattern is RRO
-- RGR 1 2
-- ROV (invalid guess) unmatchy too low
-- RVR (invalid guess!) matchy too high
-- RRG (invalid guess!) unmatchy too high
-- PRP (invalid guess!) matchy too low

-- OOR 0 2
-- OGA (invalid guess) matchy too high
