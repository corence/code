
{-# LANGUAGE TemplateHaskell #-}

module Card where

import Control.Lens
import Data.List
import Data.Maybe
import qualified Data.Map as Map
import Data.Map(Map(..))

data Event = EventDetach CardID CardID | EventAttach CardID CardID deriving (Eq, Show)
data Color = White | Blue | Black | Red | Green | Gray deriving (Eq, Show)
data Type = Creature | Player | Sorcery | Instant | Artifact | Enchantment | Mana | Life | Damage | Counter | Token | Legendary deriving (Eq, Show)
type CardName = String
type CardID = Int

data Card = Card {
    _cardID :: CardID,
    _cardName :: CardName,
    _cardColor :: [Color],
    _cardType :: [Type],
    _cardPower :: Int,
    _cardToughness :: Int,
    _cardParent :: Maybe CardID,
    _cardChildren :: [CardID]
} deriving (Eq)

makeLenses ''Card

data Board = Board {
    _boardEvents :: [Event], -- more recent events are at the head
    _boardCards :: Map CardID Card
    }

makeLenses ''Board

type Outcome a b = (Board, a) -> (Board, b)

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
