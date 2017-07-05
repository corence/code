
module Pos where

import Data.List
import Safe

data Pos = Pos Int Int
data Dir = East | Northeast | Northwest | West | Southwest | Southeast deriving (Show, Eq, Enum, Ord)
data Line = Line Pos Dir

applyAll :: a -> [a -> b] -> [b]
applyAll arg = map (\func -> func arg)

go :: Dir -> Pos -> Pos
go East (Pos x y) = Pos (x + 1) y
go West (Pos x y) = Pos (x - 1) y
go Northeast (Pos x y) = Pos x (y + 1)
go Southwest (Pos x y) = Pos x (y - 1)
go Northwest (Pos x y) = Pos (x - 1) (y + 1)
go Southeast (Pos x y) = Pos (x + 1) (y - 1)

neighboursWithSelf :: Pos -> [Pos]
neighboursWithSelf pos = pos : neighbours pos

neighbours :: Pos -> [Pos]
neighbours pos = map (\dir -> go dir pos) [(East)..Southeast]

{-
distance :: Pos -> Pos -> Maybe Int
distance pos1 pos2
  if pos1 == pos2
      then Just 0
      else bestNeighbour >>= distance pos1 >>= (+1)
          where bestNeighbour = headMay bestNeighbours
                bestNeighbours = sortBy comparePos (neighbours pos1)
                comparePos posA posB = 
-}                

-- (3,0) (3,0) -> 0
-- (3,0) (3,5) -> 5
-- (5,5) (4,4) -> 2(!)
-- (1,1) (2,0) -> 1(!)
-- (1,1) (0,2) -> 1(!)
distance :: Pos -> Pos -> Int
distance (Pos x1 y1) (Pos x2 y2)
  | y2 > y1 && x2 < x1 = 1 + distance (Pos x1 y1) (go Southeast (Pos x2 y2))
  | y2 < y1 && x2 > x1 = 1 + distance (Pos x1 y1) (go Northwest (Pos x2 y2))
  | otherwise = abs (y2 - y1) + abs (x2 - x1)
