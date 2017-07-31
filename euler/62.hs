
{-
The cube, 41063625 (345^3), can be permuted to produce two other cubes: 56623104 (384^3) and 66430125 (405^3). In fact, 41063625 is the smallest cube which has exactly three permutations of its digits which are also cube.

Find the smallest cube for which exactly five permutations of its digits are cube.
-}

import Data.List
import qualified Data.Map as Map
import Data.Map(Map(..))
import Safe

numerate :: [Int] -> Int
numerate [] = 0
numerate (digit:digits) = fromIntegral digit * (10 ^ fromIntegral (length digits)) + numerate digits

denumerate :: Int -> [Int]
denumerate 0 = []
denumerate n = denumerate (n `div` 10) ++ [n `mod` 10]

isCube x = (round (fromIntegral x ** (1/3))) ^ 3 == x

permute :: Int -> [Int]
permute = map numerate . permutations . denumerate

matchCubicPermutations :: Int -> Int -> Bool
matchCubicPermutations desiredNum = (== desiredNum) . length . filter isCube . permute

--main = putStrLn $ show $ lowestCube
--main = putStrLn $ show $ output 2 -- fail! this works even less than the first solution did

lowestCube = (take 1 . filter (matchCubicPermutations 3) . map (^3)) [1..]

type CubeRecord = Map Int [Int] -- key: sorted digits. value: list of all the unsorted forms of it

recordSortedNum :: Int -> CubeRecord -> CubeRecord
recordSortedNum num = Map.insertWith (++) sortedNum []
    where sortedNum = (numerate . sort . denumerate) num

allMatchesWithDigits :: Int -> Int -> [Int]
allMatchesWithDigits numMatches numDigits = map (minimum . snd) winners
    where candidates = [10 ^ numDigits .. 10 ^ (numDigits + 1)]
          cubicCandidates = filter isCube candidates
          records = foldr recordSortedNum (Map.fromList []) cubicCandidates
          winners = filter ((== numMatches) . length . snd) (Map.toList records)
          
output :: Int -> Int
output numMatches = headDef 0 $ concat $ map (allMatchesWithDigits numMatches) [1..9]
