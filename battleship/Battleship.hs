
module Battleship where

import qualified Data.Map as Map
import Data.Map(Map(..))
import qualified Data.Set as Set
import Data.Set(Set(..))

data Pos = Pos Int Int deriving (Show, Ord, Eq)
type Attack = Pos
data Ship = Ship [Pos]
data Board = Board {
    ships :: Set Ship,
    indexedShips :: Map Pos Ship,
    shots :: [Attack]
}

instance Eq Ship where
    Ship poses1 == Ship poses2 = head poses1 == head poses2

instance Ord Ship where
    Ship poses1 <= Ship poses2 = head poses1 <= head poses2

addShip :: Ship -> Board -> Maybe Board
addShip ship@(Ship shipPoses) board = foldr addShipCell (Just board { ships = Set.insert ship (ships board) }) shipPoses
    where addShipCell cellPos mBoard
            = case (mBoard, Map.lookup cellPos (indexedShips board)) of
                  (Just board, Nothing) -> Just board { indexedShips = Map.insert cellPos ship (indexedShips board) }
                  otherwise -> Nothing
