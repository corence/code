
{-# LANGUAGE OverloadedStrings #-}

module Events where

import qualified Data.Map as Map
import Data.Map(Map(..))

data Value = String String | Int Int | Object (Map String Value)
type Event = Map String Value
type CardSelector = Map String Value
type Card = Map String Value
type CardID = Int
type EventSelector = Map String Value
type StateQuery = Board -> Bool
type Board = [Card]

dealDamage :: Int -> CardID -> CardID -> Event
dealDamage amount source victim = Map.fromList [
    ("name", String "damage"),
    ("amount", Int amount),
    ("source", Int source),
    ("victim", Int victim)
    ]

createCard :: Card -> Event
createCard card = Map.fromList [
    ("name", String "create_card"),
    ("card", Object card)
    ]

selectorAllCreatures :: CardSelector
selectorAllCreatures = Map.fromList [
    ("type", String "Creature"),
    ("zone", String "Battlefield")
    ]

-- whenever a black creature dies, its controller gains one life.
data Trigger = Trigger EventSelector StateQuery [Event]

wheneverBlackDiesLifeIsGained :: Trigger
wheneverBlackDiesLifeIsGained = Trigger eventSelector (const True) events
    where eventSelector = Map.fromList [
                              ("name", "move"),
                              ("from", "battlefield"),
                              ("to", "graveyard"),
                              ("subject", Map.fromList [("color", "black")])
                              ]
          events = 
