{-
An ant moves on a regular grid of squares that are coloured either black or white.
The ant is always oriented in one of the cardinal directions Pos left right, up or down and moves from square to adjacent square according to the following rules:
- if it is on a black square, it flips the color of the square to white, rotates 90 degrees counterclockwise and moves forward one square.
- if it is on a white square, it flips the color of the square to black, rotates 90 degrees clockwise and moves forward one square.

Starting with a grid that is entirely white, how many squares are black after 10^18 moves of the ant?
-}

import qualified Data.Set as Set
import Data.Set(Set(..))

data Pos = Pos Int Int deriving (Eq, Show, Ord)
origin :: Pos
origin = Pos 0 0

data Direction = North | East | South | West deriving (Eq, Show, Enum)

data WorldState = WorldState (Set Pos) Pos Direction deriving (Show)
initial :: WorldState
initial = WorldState Set.empty origin North

updatePos :: Direction -> Pos -> Pos
updatePos North (Pos x y) = Pos x (y + 1)
updatePos East (Pos x y) = Pos (x + 1) y
updatePos South (Pos x y) = Pos x (y - 1)
updatePos West (Pos x y) = Pos (x - 1) y

clockwise :: Direction -> Direction
clockwise West = North
clockwise x = succ x

counterclockwise :: Direction -> Direction
counterclockwise North = West
counterclockwise x = pred x

advance :: WorldState -> WorldState
advance (WorldState blacks pos direction) = WorldState newBlacks newPos newDirection
  where onBlack = Set.member pos blacks
        newBlacks = if onBlack then Set.delete pos blacks else Set.insert pos blacks
        newPos = updatePos newDirection pos
        newDirection = if onBlack then counterclockwise direction else clockwise direction

times :: (a -> a) -> Integer -> a -> a
times _ 0 a = a
times func n a = times func (n - 1) (func a)

states = initial : map advance states
blackies (WorldState result _ _) = result

main = putStrLn $ show $ map (\x -> if x > 0 then '+' else '-') $ zipWith (-) (tail counts) counts
    where counts = map length $ map blackies $ take 600 states

--main = putStrLn $ show $ counts
    --where counts = map length $ map blackies $ take 60 states

--main = let (WorldState blackies _ _) = times advance (10 ^ 2) initial
       --in putStrLn $ show $ Set.size $ blackies
