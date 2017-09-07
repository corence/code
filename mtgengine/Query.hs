
{-# LANGUAGE TemplateHaskell #-}

module Query where

import Card
import Events
import Board
import Data.List
import BoardCards
import Control.Lens
import qualified Data.Map as Map
import Data.Map(Map(..))

-- we _might_ use something like this for convenience in queries -- for things like firebreathing it'd be handy
data BoardState = BoardState {
    board :: [Card],
    this :: Card,
    owner :: Card,
    controller :: Card
    }

makeLenses ''BoardState

type CardQuery = Board -> CardID -> Bool
type CardToInt = CardID -> Board -> Int

-- Lightning Bolt deals 3 damage to target creature or player.

isCreatureOrPlayer :: CardQuery
isCreatureOrPlayer card board = isCreature card board || isPlayer card board

isCreature :: CardQuery
isCreature cid board = isAncestor battlefield cid board && hasType Creature (boardCard board cid)

isPlayer :: CardQuery
isPlayer cid board = hasType Player (boardCard board cid)

isAncestor :: CardID -> CardQuery
isAncestor ancestor board cid = cid == ancestor || maybe False (\parent -> isAncestor ancestor board parent) (boardCard cid board ^. cardParent)

hasType :: Type -> Card -> Bool
hasType typename creature = elem typename (creature ^. cardType)

queryCards :: CardQuery -> Board -> [CardID]
queryCards query board = filter (query board) $ Map.keys (board ^. boardCards)

self :: CardID -> CardQuery
self a _ b = a == b

damage :: Int -> CardQuery -> Board -> [Event]
damage amount query board = map (EventDamage amount) victims
    where victims = queryCards query board
