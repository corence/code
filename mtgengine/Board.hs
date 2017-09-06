
{-# LANGUAGE TemplateHaskell #-}

module Board where

import Control.Lens
import Card
import qualified Data.Map as Map
import Data.Map(Map(..))
import Data.Maybe
import Data.List

data Board = Board {
    _boardEvents :: [Event], -- more recent events are at the head
    _boardCards :: Map CardID Card
    }

makeLenses ''Board


type Outcome a b = Board -> Board

boardCard :: CardID -> Board -> Card
boardCard cardID board = board ^. boardCards ^. at cardID & fromJust

updateCard :: (Card -> Card) -> CardID -> Board -> Board
updateCard = undefined

attach :: CardID -> CardID -> Board -> Board
attach parent child board
    = case boardCard child board ^. cardParent of
          Just parent -> board
          _ -> detach parent child board
               & (over boardEvents (EventAttach parent child :) . updateCard (over cardChildren (child :)) parent . updateCard (set cardParent (Just parent)) child)

detach :: CardID -> CardID -> Board -> Board
detach parent child board
    = case boardCard child board ^. cardParent of
          Just parent -> (over boardEvents (EventDetach parent child :) . updateCard (over cardChildren (delete child)) parent . updateCard (set cardParent Nothing) child) board
          _ -> board

createCard :: Card -> Board -> Board
createCard card = over boardCards (Map.insert (card ^. cardID) card)
