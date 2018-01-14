
module Rube where

import qualified Data.Vector as Vector
import qualified Data.Map as Map
import Data.Function
import Data.Bifunctor
import Data.List(intercalate)

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip (<$>)

data Pos = Pos { x :: Int, y :: Int, z :: Int } deriving (Eq, Show, Ord)
data Value = Value Int deriving (Eq, Show, Ord)
data SpinDirection = Clockwise | Counterclockwise deriving (Show, Eq)
data Rube = Rube { nodes :: Map.Map Pos Value } -- a rubiks cube. For now, 3x3. But in future, NxN.

origin = Pos 0 0 0

data Direction
  = Skyward
  | North
  | East
  | South
  | West
  | Hellbound
  deriving (Show, Eq)

data Rotation
  = RollNorth
  | RollSouth
  | RollEast
  | RollWest
  | TurnWest
  | TurnEast
  deriving (Show, Eq)

skywardRing :: [Pos]
skywardRing
  = iterate (map (rotatePos TurnEast)) [Pos 1 1 1, Pos 1 1 0]
  & take 4
  & concat

skywardFace :: [Pos]
skywardFace = [Pos x 1 z | x <- [(-1), 0, 1], z <- [(-1), 0, 1]]

allRubePoses :: [Pos]
allRubePoses = do
  let options = [(-1), 0, 1]
  x <- options
  y <- options
  z <- options
  (pure $ Pos x y z) & filter (/= origin)

standardRube :: Rube
standardRube
  = zip allRubePoses (map Value [0..])
  & Map.fromList
  & Rube

facePoses :: Direction -> [Pos]
facePoses direction = map roll skywardFace
  where roll = case direction of
                      Hellbound -> rotatePos RollNorth . rotatePos RollNorth -- this time, we want to roll around the x axis so the top-left is where we expect
                      South     -> rotatePos RollSouth
                      North     -> rotatePos TurnEast . rotatePos TurnEast . rotatePos RollNorth
                      East      -> rotatePos TurnWest . rotatePos RollEast
                      West      -> rotatePos TurnEast . rotatePos RollWest
                      Skyward   -> id

faceRing :: Direction -> [Pos]
faceRing direction = map (rotateFromSky direction) skywardRing

rotateFromSky :: Direction -> (Pos -> Pos)
rotateFromSky targetDirection
  = case targetDirection of
           Hellbound -> rotatePos RollNorth . rotatePos RollNorth -- chosen arbitrarily, but we could have gone down there with any of 4 paths
           South     -> rotatePos RollSouth
           East      -> rotatePos RollEast
           West      -> rotatePos RollWest
           North     -> rotatePos RollNorth
           Skyward   -> id

-- move from one pos to another pos on the rube, by translocating it to another direction relative to the center of the cube
rotatePos :: Rotation -> Pos -> Pos
rotatePos rotation pos@(Pos x y z)
  = case rotation of
         RollNorth -> Pos x (-z) y
         RollSouth -> Pos x z (-y)
         RollEast  -> Pos y (-x) z
         RollWest  -> Pos (-y) x z
         TurnWest  -> Pos (-z) y x
         TurnEast  -> Pos z y (-x)

rotateFace :: Direction -> SpinDirection -> Rube -> Rube
rotateFace faceDirection spinDirection rube@(Rube rNodes)
  = foldr setValue rube (zip destPoses sourceValues)
  where setValue (destPos, destValue) (Rube nodes) = Rube (Map.insert destPos destValue nodes)
        destPoses = map (rotatePos rotation) sourcePoses
        sourceValues = map (rNodes Map.!) sourcePoses
        sourcePoses = faceRing faceDirection
        rotation = faceRotation faceDirection spinDirection

faceRotation :: Direction -> SpinDirection -> Rotation
faceRotation faceDirection spinDirection
  = case spinDirection of
            Clockwise -> case faceDirection of
                              Hellbound -> TurnWest
                              Skyward   -> TurnEast
                              South     -> RollEast
                              North     -> RollWest
                              East      -> RollNorth
                              West      -> RollSouth
            Counterclockwise -> case faceDirection of
                              Hellbound -> TurnEast
                              Skyward   -> TurnWest
                              South     -> RollWest
                              North     -> RollEast
                              East      -> RollSouth
                              West      -> RollNorth

instance Show Rube where
  show (Rube nodes)
    = showAsList ++ "\n" ++ showAsSquares
    where showAsList = nodes & Map.toList & show
          showAsSquares = ([Skyward, North, East, South, West, Hellbound]
                        & map facePoses
                        & map (showFace nodes)
                        & intercalate "\n\n")

showFace :: Map.Map Pos Value -> [Pos] -> String
showFace nodes poses
  = poses
  & map (nodes Map.!)
  & map (\(Value v) -> v)
  & rows 3
  & map show
  & unlines

rows :: Int -> [a] -> [[a]]
rows _ [] = []
rows rowLength list = take rowLength list : rows rowLength (drop rowLength list)

{-
    show (Rube nodes) = map showFace [
        (Pos (-1) 1 1, Pos 1 0 0, Pos 0 0 (-1)), -- skyward face
        (Pos 1 1 1, Pos (-1) 0 0, Pos 0 (-1) 0), -- north face
        (Pos 1 1 (-1), Pos 0 0 1, Pos 0 (-1) 0), -- east face
        (Pos (-1) 1 (-1), Pos 1 0 0, Pos 0 (-1) 0), -- south face
        (Pos (-1) 1 1, Pos 0 0 (-1), Pos 0 (-1) 0), -- west face
        (Pos (-1) (-1) (-1), Pos 1 0 0, Pos 0 0 1) -- hellbound face
        ]
        where showFace (startPos, horzScan, vertScan) = undefined
-}
