
module Zone
( Zone (..)
, Pos (..)
, add
, amalgamate
, contains
, create_container
, create_square
, extend
, from_pos
, intersection
, overlaps
, pos_distance
) where

type Pos = [Int]
data Zone = ZVoid | Zone Pos Pos deriving (Show, Eq)

void = ZVoid

from_pos :: Pos -> Zone
from_pos pos = Zone pos pos

pos_distance :: Pos -> Pos -> Int
pos_distance [x1, y1] [x2, y2] = abs(x1 - x2) + abs(y1 - y2)

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

-- creates a zone big enough for all of these Pos
create_container :: [Pos] -> Zone
create_container [] = void
create_container (p:ps) = extend p (create_container ps)

-- creates a square
create_square :: Pos -> Int -> Zone
create_square center radius = Zone nw se
    where [cx, cy] = center
          nw = [cx - radius, cy - radius]
          se = [cx + radius, cy + radius]

intersection :: Zone -> Zone -> Zone
intersection (Zone [x1, y1] [x2, y2]) (Zone [x3, y3] [x4, y4])
    = Zone (map maximum [[x1, x3], [y1, y3]])
           (map minimum [[x2, x4], [y2, y4]])

intersection2 :: [Zone] -> Zone
intersection2 zones = Zone [maximum ws, maximum ns] [minimum es, minimum ss]
    where ws = map (\(Zone [w, _] [_, _]) -> w) zones
          ns = map (\(Zone [_, n] [_, _]) -> n) zones
          es = map (\(Zone [_, _] [e, _]) -> e) zones
          ss = map (\(Zone [_, _] [_, s]) -> s) zones

intersection3 :: [Zone] -> Zone
intersection3 zones = Zone [maximum ws, maximum ns] [minimum es, minimum ss]
    where (ws, ns, es, ss)
            = foldr
              (\(Zone [w, n] [e, s]) (ws, ns, es, ss) -> (w : ws, n : ns, e : es, s : ss))
              ([], [], [], [])
              zones
