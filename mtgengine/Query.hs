
module Query where

import Card

--class BoolQuery q where
    --queryBool :: q -> Bool

--class IntQuery q where
    --queryInt :: q -> Int

--class CardQuery q where
    --queryCards :: q -> [Card]

type CardFilter = Card -> Bool
type CardToInt = Card -> Int

-- Lightning Bolt deals 3 damage to target creature or player.

isCreatureOrPlayer :: CardFilter
isCreatureOrPlayer card = isCreature card || isPlayer card

isCreature :: CardFilter
isCreature card = hasAncestor battlefield card && hasType Creature card

isPlayer :: CardFilter
isPlayer card = hasType Player card

hasAncestor :: Card -> CardFilter
hasAncestor ancestor card = card == ancestor || maybe False (hasAncestor ancestor) (cardParent card)

queryCards :: CardFilter -> Board -> [Card]
--queryCards filter board = 
