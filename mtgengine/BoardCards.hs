
module BoardCards where

import Board
import Card
import qualified Data.Map as Map
import Data.Map(Map(..))
import Data.List

([stack, battlefield], boardWithZones) = createZones defaultBoard
    where defaultBoard = Board { _boardEvents = [], _boardCards = Map.empty, _nextCardID = 1 }

createZones :: Board -> ([CardID], Board)
createZones board = foldl' nameToCard ([], board) ["Battlefield", "Stack"]
    where card = defaultCard { _cardType = [Zone] }
          nameToCard (cids, board) name = let (cid, newBoard) = createCard card { _cardName = name } board in (cid : cids, newBoard)
