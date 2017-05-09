
-- https://en.wikibooks.org/wiki/Haskell/Lenses_and_functional_references

-- A point in the plane.
data Point = Point
    { positionX :: Double
    , positionY :: Double
    } deriving (Show)

-- A line segment from one point to another.
data Segment = Segment
    { segmentStart :: Point
    , segmentEnd :: Point
    } deriving (Show)

-- Helpers to create points and segments.
makePoint :: (Double, Double) -> Point
makePoint (x, y) = Point x y

makeSegment :: (Double, Double) -> (Double, Double) -> Segment
makeSegment start end = Segment (makePoint start) (makePoint end)
