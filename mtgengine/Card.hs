
{-# LANGUAGE TemplateHaskell #-}

module Card where

import Control.Lens

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

