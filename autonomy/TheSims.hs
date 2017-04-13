
-- This is a simplified model of The Sims (1999 game). A few simplifications in the current model are:
-- 1) time does not exist -- walking and actions are instant
-- 2) occlusion does not exist -- all objects can exist in the same place
-- 3) the y and z dimensions do not exist; actor positions are expressed as a single Int
-- 4) items can't exist on the floor -- they need to be held by an object (a person, a table, an oven)
-- 5) needs do not increase over time
-- 6) there's only one need and only one way to resolve it :^)

module TheSims
( Command(..)
, World(..)
, actor_exists
, be_at
, be_unhungry
, cook
, dump_world
, eat
, have_food
, have_item
, query_actor
, seek_item
, take_item
, unhungry
) where

import Intents
import qualified Actor as Actor
import Actor(ActorID, Actor(..), ItemID, Pos)
import qualified Data.Map as Map
import Data.Map(Map(..))
import Control.Monad.State

type World = Map ActorID Actor

dump_world :: World -> String
dump_world world = dump_actors $ Map.elems world

dump_actors :: [Actor] -> String
dump_actors actors = (actors >>= ("\n    " ++) . show) -- ++ "\n"

find_actor :: ActorID -> World -> Actor
find_actor actor_id world = case Map.lookup actor_id world of
                               Just actor -> actor
                               Nothing -> error $ "find_actor: actor not found: " ++ show actor_id

actor_exists :: ActorID -> World -> Bool
actor_exists = Map.member

find_actors_with :: (Actor -> Bool) -> World -> [Actor]
find_actors_with predicate world = filter predicate (Map.elems world)

query_actor :: ActorID -> (Actor -> a) -> World -> a
query_actor actor_id query world = query (find_actor actor_id world)

adjust_actor :: ActorID -> (Actor -> Actor) -> World -> World
adjust_actor actor_id adjust world = Map.insert actor_id (adjust (find_actor actor_id world)) world

adjust_actors :: [(ActorID, (Actor -> Actor))] -> World -> World
adjust_actors [] world = world
adjust_actors ((actor_id, adjust) : adjusters) world = adjust_actors adjusters new_world
    where new_world = adjust_actor actor_id adjust world

--replace_actor :: Actor -> World -> World
--replace_actor actor world = Map.insert (actor_id actor) actor world

hungry :: Actor -> Bool
hungry actor = Actor.get_item "hunger" actor >= 20

unhungry :: Actor -> Bool
unhungry actor = not $ hungry actor

be_unhungry :: ActorID -> Goal World
be_unhungry actor_id = Goal "be_unhungry" [\_ -> [eat actor_id]] [query_actor actor_id unhungry]

eat :: ActorID -> Task World
eat actor_id = Task "eat" [have_food 1 actor_id] [consume_food] (const 1)
    where consume_food world = adjust_actors [(actor_id, Actor.sub_item "hunger" 10), (actor_id, Actor.sub_item "food" 1)] world

have_food :: Int -> ActorID -> Goal World
have_food amount actor_id = have_item "food" amount [generate_cook_tasks actor_id] actor_id

have_item :: ItemID -> Int -> [World -> [Task World]] -> ActorID -> Goal World
have_item item_id amount task_generators actor_id = Goal "have_item" (seek_item item_id actor_id [actor_id] : task_generators) [already_has_item] -- TODO: generators!
    where already_has_item = (query_actor actor_id (\actor -> Actor.get_item item_id actor >= amount))

-- 2 ways we can do this:
-- 1) foreach oven, generate a mission to cook food in it
-- 2) generate one mission that will constantly scan for ovens and move closer to them
-- we picked the first one
generate_cook_tasks :: ActorID -> World -> [Task World]
generate_cook_tasks actor_id world = map (\oven_id -> cook oven_id actor_id) oven_ids
    where oven_ids = map Actor.get_id (find_actors_with (\actor -> Actor.get_item "oven" actor >= 1) world)

cook :: ActorID -> ActorID -> Task World
cook oven_id actor_id
    = Task ("cook in oven " ++ show oven_id) [have_item "prepped_ingredients" 1 [] actor_id, be_at oven_id actor_id] [make_food oven_id] (const 1) -- TODO: mission generators. Also this should be looked up from somewhere -- surely "cook" isn't the place to encode all the ways to find prepped_ingredients
      -- make_food: decrement the actor's prepped ingredients; increment the oven's food
    where make_food oven_id world = adjust_actors [
                                          (actor_id, Actor.sub_item "prepped_ingredients" 1),
                                          (oven_id, Actor.add_item "food" 1)
                                          ] world

seek_item :: ItemID -> ActorID -> [ActorID] -> World -> [Task World]
seek_item item_id actor_id blacklist world = take_item_tasks
    where actors_with_item = filter (\target -> Actor.get_item item_id target > 0) (Map.elems world)
          aids_with_item = map Actor.get_id actors_with_item
          take_item_tasks = map (\target -> take_item item_id target actor_id) aids_with_item

take_item :: ItemID -> ActorID -> ActorID -> Task World
take_item item_id target_id actor_id = Task name [be_at target_id actor_id] [exchange] (const 0)
    where name = ("take_item '" ++ item_id ++ "' from " ++ show target_id)
          exchange world = adjust_actors [(actor_id, (Actor.add_item item_id 1)), (target_id, (Actor.sub_item item_id 1))] world

be_at :: ActorID -> ActorID -> Goal World
be_at target_id actor_id = Goal ("be at " ++ show target_id) [\_ -> [go_toward target_id actor_id]] [same_place]
    where same_place world = query_actor actor_id Actor.get_pos world == query_actor target_id Actor.get_pos world

{-
be_at_actor_with :: ItemID -> Int -> ActorID -> Task
be_at_actor_with item_id amount actor_id = Task any_matches Nothing missions
    where any_matches world = let actor = find_actor actor_id world in any (\target_id -> actor_matches actor (find_actor target_id world))
          actor_matches actor target = Actor.get_pos actor == Actor.get_pos target && actor_inv item_id target >= amount && actor_id target /= actor_id actor
          missions world = map (\target_id -> go_toward target_id actor_id) (target_ids world)
          target_ids world = map actor_id $ filter (\target -> 
-}

go_toward :: ActorID -> ActorID -> Task World
go_toward actor_id target_id = Task "go toward" [] [update_pos] (\world -> fromIntegral $ Actor.distance (old_pos world) (new_pos world))
    where new_pos world = query_actor target_id Actor.get_pos world
          old_pos world = query_actor actor_id Actor.get_pos world
          update_pos world = adjust_actor actor_id (Actor.set_pos (new_pos world)) world

