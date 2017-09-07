
{-# LANGUAGE TemplateHaskell #-}

module Board where

import Control.Lens
import Card
import qualified Data.Map as Map
import Events
import Data.Map(Map(..))
import Data.Maybe
import Data.List

data Board = Board {
    _boardEvents :: [Event], -- more recent events are at the head
    _boardCards :: Map CardID Card,
    _nextCardID :: CardID
    }

makeLenses ''Board


boardCard :: CardID -> Board -> Card
boardCard cardID board = board ^. boardCards ^. at cardID & fromJust

updateCard :: (Card -> Card) -> CardID -> Board -> Board
updateCard = undefined

attach :: CardID -> CardID -> Board -> Board
attach parent child board
    = case boardCard child board ^. cardParent of
          Just parent -> board
          _ -> detachOldParent & addEvent & updateParent & updateChild
      where detachOldParent = detach parent child board
            addEvent = over boardEvents (EventAttach parent child :)
            updateParent = updateCard (over cardChildren (child :)) parent
            updateChild = updateCard (set cardParent (Just parent)) child

detach :: CardID -> CardID -> Board -> Board
detach parent child board
    = case boardCard child board ^. cardParent of
          Just parent -> board & addEvent & updateParent & updateChild
          _ -> board
      where addEvent = over boardEvents (EventDetach parent child :)
            updateParent = updateCard (over cardChildren (delete child)) parent
            updateChild = updateCard (set cardParent Nothing) child

createCard :: Card -> Board -> (CardID, Board)
createCard card board = (identifier, board & updateID & addCard)
    where cardWithID = set cardID identifier card
          identifier = board ^. nextCardID
          updateID = over nextCardID (+1)
          addCard = over boardCards (Map.insert identifier cardWithID)
