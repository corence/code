module Main where

import Rube
import Data.Function

main :: IO ()
main = do
  print $ countIterations (rotateFace East Counterclockwise) standardRube standardRube
  print $ countIterations (rotateFace Skyward Counterclockwise . rotateFace West Clockwise) standardRube standardRube

countIterations :: (Rube -> Rube) -> Rube -> Rube -> Int
countIterations mutator startingRube goalRube
  = iterate mutator startingRube
  & drop 1
  & takeWhile (/= goalRube)
  & length
