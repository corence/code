
{-# LANGUAGE TemplateHaskell #-}

module Card where

import Control.Lens

data Event = EventDetach CardID CardID | EventAttach CardID CardID deriving (Eq, Show)
data Color = White | Blue | Black | Red | Green | Gray deriving (Eq, Show)
data Type = Creature | Player | Sorcery | Instant | Artifact | Enchantment | Mana | Life | Damage | Counter | Token | Zone | Step | Legendary deriving (Eq, Show)
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
    _cardChildren :: [CardID],
    _cardSource :: Maybe CardID,
    _cardOwner :: Maybe CardID,
    _cardController :: Maybe CardID
} deriving (Eq, Show)

makeLenses ''Card

defaultCard = Card {
    _cardID = 0,
    _cardName = "",
    _cardColor = [],
    _cardType = [],
    _cardPower = 0,
    _cardToughness = 0,
    _cardParent = Nothing,
    _cardChildren = [],
    _cardSource = Nothing,
    _cardOwner = Nothing,
    _cardController = Nothing
    }

