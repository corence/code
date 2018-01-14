
module Cube where

import qualified Data.Vector as Vector
import qualified Data.Map as Map
import Data.Function
import Data.Bifunctor

-- we can either model this as a set of Squares, each grouped into 6 Faces...
-- or we can model it as a group of cubes, each with a pos
-- with the second option, we are choosing not to track the rotation of each minicube, though we could do that.
-- it's probably only relevant for face-central cubes.

{-
data AbsoluteDirection
 = Skyward
 | North
 | East
 | South
 | West
 | Hellbound

data RelativeDirection
 = Up
 | Forward
 | Right
 | Back
 | Left
 | Down

data Rotation
 = PitchDown
 | PitchUp
 | RollLeft
 | RollRight
 | TurnLeft
 | TurnRight

data AbsolutePos = AbsolutePos { x :: Int, y :: Int, z :: Int }

dirToPos :: AbsoluteDirection -> Pos
dirToPos direction
  = case direction of
    Skyward   -> Pos 0 1 0
    Hellbound -> Pos 0 0 1
    East      -> Pos 1 0 0
    South     -> Pos 0 0 (-1)
    West      -> Pos (-1) 0 0
    Down      -> Pos 0 (-1) 0

advance :: AbsoluteDirection -> Pos -> Pos
advance direction pos = add (dirToPos direction) pos

data Orientation = Orientation Direction Direction Direction

forth :: Orientation -> Pos -> Pos
forth (Orientation _ _ direction) pos = advance direction pos

-- rotates a pos around the 3x3 rube cube
rotatePos :: Rotation -> Pos -> Pos
rotatePos rotation (Pos x y z)
  = case rotation of
    PitchUp -> Pos `

turnRight :: Orientation -> Orientation
turnRight (Orientation x y z) = Orientation (aboutFace z) y x

turnLeft :: Orientation -> Orientation
turnLeft (Orientation x y z) = Orientation z y (aboutFace x)

pitchDown :: Orientation -> Orientation
pitchDown (Orientation x y z) = Orientation x z (aboutFace y)

pitchUp :: Orientation -> Orientation
pitchUp (Orientation x y z) = Orientation x (aboutFace z) y

rollLeft :: Orientation -> Orientation
rollLeft (Orientation x y z) = Orientation y (aboutFace x) z

rollRight :: Orientation -> Orientation
rollRight (Orientation x y z) = Orientation (aboutFace y) x z

data FaceRotation
 = Clockwise
 | Counterclockwise

type CellID = Int
data RubiksCube = RubiksCube (Map.Map Pos CellID)

rotate1 :: Direction -> FaceRotation -> RubiksCube -> RubiksCube
rotate1 faceID rotation cube
  = case rotation of
      Clockwise        -> setValues poses clockwiseValues cube
      Counterclockwise -> setValues poses counterClockwiseValues cube
    where poses = getFaceRing faceID
          cells = getCellIDs poses cube
          clockwiseCells = cycle cells & drop 2 & take (length values)
          counterclockwiseCells = cycle cells & drop (length values - 2) & take (length values)

getFaceRing :: Direction -> [Pos]
getFaceRing direction
  = case direction of
  Skyward -> [Pos 0 2 0,
              Pos 0 2 1,
              Pos 0 2 2,
              Pos 1 2 2,
              Pos 2 2 2,
              Pos 2 2 1,
              Pos 2 2 0,
              Pos 1 2 0]
  North -> [Pos 
-}

{-
getFaceRing orientation pos = [
    pos,
    forth orientation pos,
    forth orientation (forth orientation pos)
    ]
    -}

{-
-- rotateAroundOrigin :: Rotation -> Pos -> Pos

-- rotate a face. If it's the west face and we say to pitch down, then
-- rotateFaceRelative :: Direction -> ClockWisdom -> Rube -> Rube
-- 1) using the Direction, pick the poses that will be rotated, clockwise. To do this, hardcode the top ring, then rotate its positions using rotateAroundOrigin.
-- 2) pick the Rotation. To do this, maybe just hardcode the 12 combinations of Direction and ClockWisdom.
-- 3) call rotateAroundOrigin on each Pos, to discover where the value should be transferred.
-- 4) actually rotate the cell

data Pos = Pos { x :: Int, y :: Int, z :: Int } deriving (Eq, Show, Ord)
data Value = Value Int deriving (Eq, Show)
data ClockWisdom = Clockwise | Counterclockwise
data Rube = Rube (Map.Map Pos Value)
type Direction = Pos
type Rotation = Pos

rotatePos :: Direction -> Direction -> Pos -> Pos
rotatePos oldDirection newDirection pos@(Pos x y z)
  | oldDirection == newDirection = pos
  | oldDirection /= skyward && newDirection /= skyward = pos & rotatePos oldDirection skyward & rotatePos skyward newDirection
  | oldDirection == north     = Pos x z y
  | oldDirection == south     = Pos x (-z) y
  | oldDirection == east      = Pos (-y) x z
  | oldDirection == west      = Pos y x z
  | oldDirection == hellbound = Pos x (-y) (-z)

rotatePosBy :: Rotation -> Pos -> Pos
rotatePosBy rotation@(Pos rx ry rz) pos@(Pos x y z)
  = (pitchDown . pitchUp) pos
  where pitchDown (Pos x y z) = 

skyward = Pos 0 1 0

rotationBetween :: Direction -> Direction -> Rotation
rotationBetween dir1 dir2
  | dir1 == dir2 = NoRotation
  | dir2 == Skyward = rotationBetween dir1 Skyward `with` rotationBetween Skyward dir2

setNode :: Value -> Pos -> Rube -> Rube
setNode value pos (Rube nodes) = Rube $ Map.insert pos value nodes

-- terminology: (to avoid confusingly saying "rotate" for multiple things)
-- "spin": rotating all of the nodes of one face
-- "rotate": going to a new Pos from an old Pos by moving around the origin (aka the center of the cube)

spinFaceNodes :: Direction -> ClockWisdom -> Rube -> Rube
spinFaceNodes direction clockwisdom rube
  = foldr (\(pos, value) -> setNode value pos) rube newValues
    where newValues = map (getValue rube) facePoses & spinValues clockwisdom & zip facePoses
          facePoses = topFacePoses & map (rotatePos skyward direction)
-}
