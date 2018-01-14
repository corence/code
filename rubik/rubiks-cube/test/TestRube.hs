
import Rube
import Test.Hspec
import Test.QuickCheck
import Data.Function
import qualified Data.Map as Map

instance Eq Rube
  where (==) (Rube nodes1) (Rube nodes2) = nodes1 == nodes2

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
