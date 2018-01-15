
{-# LANGUAGE ScopedTypeVariables #-}

import Rube
import Test.Hspec
import Test.QuickCheck
import Data.Function
import qualified Data.Map as Map
import Data.List(sort)
import Data.Map((!))

instance Arbitrary Rube
  where arbitrary = do
          values <- [0..] & take (length allRubePoses) & map Value & shuffle
          let entries = zip allRubePoses values
          pure $ Rube (Map.fromList entries)

instance Arbitrary Direction
  where arbitrary = oneof $ map pure $ [Skyward, North, East, South, West, Hellbound]

instance Arbitrary SpinDirection
  where arbitrary = oneof [pure Clockwise, pure Counterclockwise]

main :: IO ()
main = hspec $ do
  describe "consistency of rotating faces" $ do
    it "should be a no-op to rotate any face 4 times" $
      property $ \rube faceDirection spinDirection -> iterate (rotateFace faceDirection spinDirection) rube & (!! 4) & (== rube)

    it "should be a no-op to rotate a face n times, then unrotate it n times" $ do
      property $ \rube faceDirection numSpins ->
                        let spins = (repeat Clockwise & take numSpins) ++ (repeat Counterclockwise & take numSpins)
                        in foldr (rotateFace faceDirection) rube spins & (== rube)

    it "should *not* be a no-op to rotate any face once" $ do
      property $ \rube faceDirection spinDirection -> rotateFace faceDirection spinDirection rube /= rube

    it "should have the same set of Values after a rotation" $ do
      property $ \rube faceDirection spinDirection ->
                    let values (Rube nodes) = Map.toList nodes & map snd
                    in rotateFace faceDirection spinDirection rube
                    & values
                    & sort
                    & (== sort (values rube))

    it "should move the Pos to the expected position after multiple rotations" $ do
      property $ \rube ->
          let rotatedRube = rube
                          & rotateFace East Clockwise
                          & rotateFace Hellbound Clockwise
                          & rotateFace West Counterclockwise
          in (nodes rotatedRube ! Pos (-1) (-1) (-1)) == (nodes rube ! Pos 1 1 1)

    it "should never reposition the face-center values" $ do
      property $ \rube rotations faceToCheck ->
                        let rotatedRube = foldr (uncurry rotateFace) rube (rotations :: [(Direction, SpinDirection)])
                        in (nodes rotatedRube ! dirToPos faceToCheck) == (nodes rube ! dirToPos faceToCheck)
  describe "solver" $ do
    -- it "should solve any arbitrary rube" $ do
      -- property $ not . null . solve 5 standardRube
    it "should solve a lightly-permuted rube" $ do
      property $ \(path :: Path) -> length path <= 4 ==> (not . null . solve 4 standardRube) (foldr (uncurry rotateFace) standardRube path)

dirToPos :: Direction -> Pos
dirToPos Hellbound = Pos 0 (-1) 0
dirToPos West      = Pos (-1) 0 0
dirToPos South     = Pos 0 0 (-1)
dirToPos East      = Pos 1 0 0
dirToPos North     = Pos 0 0 1
dirToPos Skyward   = Pos 0 1 0
