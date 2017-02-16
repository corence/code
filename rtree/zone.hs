
module Zone
( Zone (..)
, Pos (..)
, from_pos
, contains
, overlaps
, extend
, add
, amalgamate
) where

type Pos = [Int]
data Zone = ZVoid | Zone Pos Pos deriving (Show, Eq)

void = ZVoid

from_pos :: Pos -> Zone
from_pos pos = Zone pos pos

contains :: Pos -> Zone -> Bool
contains _ ZVoid = False
contains pos (Zone nw se) = and (zipWith (<=) nw pos) && and (zipWith (>=) se pos)

overlaps :: Zone -> Zone -> Bool
overlaps ZVoid _ = False
overlaps _ ZVoid = False
overlaps (Zone [n1, w1] [s1, e1]) (Zone [n2, w2] [s2, e2]) = 
  not $ (e1 < w2) || (w1 > e2) || (s1 < n2) || (n1 > s2)

extend :: Pos -> Zone -> Zone
extend pos ZVoid = Zone pos pos
extend pos (Zone nw se) = Zone most_nw most_se
  where most_nw = zipWith min nw pos
        most_se = zipWith max se pos

add :: Zone -> Zone -> Zone
add ZVoid zone = zone
add zone ZVoid = zone
add zone (Zone nw se) = extend se (extend nw zone)

amalgamate :: [Zone] -> Zone
amalgamate = foldr (\zone1 zone2 -> add zone1 zone2) ZVoid

