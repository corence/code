
import Test.QuickCheck
import Signpost

type PuzzleCell = (CellID, Int, [CellID])
data TestBoard = TestBoard [PuzzleCell]

instance Arbitrary Chain where
  arbitrary = do
      _cid <- arbitrary
      return Chain {
          cid = _cid,
          chainCells = [_cid],
          chainValue = 0,
          chainLength = 1,
          chainOutputs = [],
          chainInputs = []
      }
      
import Test.QuickCheck

takeFromList :: [a] -> Gen a
takeFromList xs =
    choose (0, length xs - 1) >>= \i -> return $ xs !! i

-- generate arbitrary chains
-- more specifically, we generate an arbitrary puzzle
-- more specifically:
--   -- generate a bunch of cell IDs
--   -- set one with value 1 and one with value `length`
--   -- randomly associate them (respecting value)
instance Arbitrary TestBoard where
  arbitrary = do
      board <- arbitrary
      return TestBoard $ associate 
      
prop_getChain_gets_chains_from_board :: Chain -> [Chain] -> Property
prop_getChain_gets_chains_from_board replacement board
    = forAll (elements board) $ \c -> elem c (replaceChain (cid replacement) replacement board)

main = quickCheck prop_getChain_gets_chains_from_board
-- /show
