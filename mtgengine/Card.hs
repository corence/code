
module Card where

type CardName = String
data Card = Card {
    cardName :: CardName,
    cardColor :: [Color],
    cardType :: [Type],
    cardPower :: Int,
    cardToughness :: Int,
    cardParent :: Maybe Card,
    cardChildren :: [Card]
}

data Color = White | Blue | Black | Red | Green | Gray
data Type = Creature | Player | Sorcery | Instant | Artifact | Enchantment | Mana | Life | Damage | Counter | Token | Legendary

data Board = Board [Card] -- and the event log

attach :: (Card, Card) -> (Card, Card)
attach (parent, child) = (parent
