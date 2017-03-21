
module Actor
( Actor(..)
, ActorID
, ItemID
, Pos
, add_item
, get_id
, get_item
, get_pos
, set_item
, set_pos
, sub_item
) where

import Intents
import qualified Data.Map as Map
import Data.Map(Map(..))

type ActorID = Int -- identifies a specific actor instance
data Actor = Actor ActorID Pos (Map ItemID Int) deriving Show -- id, pos, inventory
type Pos = Int
type ItemID = String -- identifies a class of items. items are anything that can be owned in multiples, like "food" or "x_position" or "hunger" or "reputation"

get_id :: Actor -> ActorID
get_id (Actor aid _ _) = aid

get_pos :: Actor -> Pos
get_pos (Actor _ pos _) = pos

set_pos :: Pos -> Actor -> Actor
set_pos new_pos (Actor aid _ inventory) = Actor aid new_pos inventory

get_item :: ItemID -> Actor -> Int
get_item item_id (Actor _ _ inventory) = Map.findWithDefault 0 item_id inventory

set_item :: ItemID -> Int -> Actor -> Actor
set_item item_id amount (Actor aid pos inventory) = Actor aid pos (Map.insert item_id amount inventory)

add_item :: ItemID -> Int -> Actor -> Actor
add_item item_id amount actor = set_item item_id (amount + get_item item_id actor) actor

sub_item :: ItemID -> Int -> Actor -> Actor
sub_item item_id amount actor = add_item item_id (negate amount) actor

