
module IntentSims
( Command(..)
, State(..)
, be_unhungry
, eat
, have_food
, have_item
, seek_item
) where

import Intents
import qualified Actor as Actor
import Actor(ActorID, Actor(..), ItemID, Pos)
import qualified Data.Map as Map
import Data.Map(Map(..))

type State = Map ActorID Actor
type Command = State -> State

find_actor :: ActorID -> State -> Actor
find_actor actor_id state = case Map.lookup actor_id state of
                               Just actor -> actor
                               Nothing -> error $ "find_actor: actor not found: " ++ show actor_id

find_actors_with :: (Actor -> Bool) -> State -> [Actor]
find_actors_with predicate state = filter predicate (Map.elems state)

query_actor :: ActorID -> (Actor -> a) -> State -> a
query_actor actor_id query state = query (find_actor actor_id state)

adjust_actor :: ActorID -> (Actor -> Actor) -> State -> State
adjust_actor actor_id adjust state = Map.insert actor_id (adjust (find_actor actor_id state)) state

adjust_actors :: [(ActorID, (Actor -> Actor))] -> State -> State
adjust_actors [] state = state
adjust_actors ((actor_id, adjust) : adjusters) state = adjust_actors adjusters new_state
    where new_state = adjust_actor actor_id adjust state

--replace_actor :: Actor -> State -> State
--replace_actor actor state = Map.insert (actor_id actor) actor state

hungry :: Actor -> Bool
hungry actor = Actor.get_item "hunger" actor >= 20

unhungry :: Actor -> Bool
unhungry actor = not $ hungry actor

be_unhungry :: ActorID -> Goal Command State
be_unhungry actor_id = Goal "be_unhungry" [\_ -> [eat actor_id]] [query_actor actor_id unhungry]

eat :: ActorID -> Task Command State
eat actor_id = Task "eat" [have_food 1 actor_id] [consume_food]
    where consume_food state = adjust_actors [(actor_id, Actor.sub_item "hunger" 10), (actor_id, Actor.sub_item "food" 1)] state

have_food :: Int -> ActorID -> Goal Command State
have_food amount actor_id = have_item "food" amount [generate_cook_tasks actor_id] actor_id

have_item :: ItemID -> Int -> [State -> [Task Command State]] -> ActorID -> Goal Command State
have_item item_id amount task_generators actor_id = Goal "have_item" (seek_item item_id actor_id [actor_id] : task_generators) [already_has_item] -- TODO: generators!
    where already_has_item = (query_actor actor_id (\actor -> Actor.get_item item_id actor >= amount))

-- 2 ways we can do this:
-- 1) foreach oven, generate a mission to cook food in it
-- 2) generate one mission that will constantly scan for ovens and move closer to them
-- we picked the first one
generate_cook_tasks :: ActorID -> State -> [Task Command State]
generate_cook_tasks actor_id state = map make_task oven_ids
    where oven_ids = map Actor.get_id (find_actors_with (\actor -> Actor.get_item "oven" actor >= 1) state)
          make_task oven_id = Task "cook in this oven" [have_item "prepped_ingredients" 1 [] actor_id, be_at oven_id actor_id] [make_food oven_id] -- TODO: mission generators
          -- make_food: decrement the actor's prepped ingredients; increment the oven's food
          make_food oven_id state = adjust_actors [
                                                  (actor_id, Actor.sub_item "prepped_ingredients" 1),
                                                  (oven_id, Actor.add_item "food" 1)
                                                  ] state

seek_item :: ItemID -> ActorID -> [ActorID] -> State -> [Task Command State]
seek_item item_id actor_id blacklist state = take_item_tasks
    where actors_with_item = filter (\target -> Actor.get_item item_id target > 0) (Map.elems state)
          aids_with_item = map Actor.get_id actors_with_item
          take_item_tasks = map (\target -> take_item item_id target actor_id) aids_with_item

take_item :: ItemID -> ActorID -> ActorID -> Task Command State
take_item item_id target_id actor_id = Task name [be_at target_id actor_id] [exchange]
    where name = ("take_item '" ++ item_id ++ "' from " ++ show target_id)
          exchange state = adjust_actors [(actor_id, (Actor.add_item item_id 1)), (target_id, (Actor.sub_item item_id 1))] state

be_at :: ActorID -> ActorID -> Goal Command State
be_at target_id actor_id = Goal ("be at " ++ show target_id) [\_ -> [go_toward target_id actor_id]] [same_place]
    where same_place state = query_actor actor_id Actor.get_pos state == query_actor target_id Actor.get_pos state

{-
be_at_actor_with :: ItemID -> Int -> ActorID -> Task
be_at_actor_with item_id amount actor_id = Task any_matches Nothing missions
    where any_matches state = let actor = find_actor actor_id state in any (\target_id -> actor_matches actor (find_actor target_id state))
          actor_matches actor target = Actor.get_pos actor == Actor.get_pos target && actor_inv item_id target >= amount && actor_id target /= actor_id actor
          missions state = map (\target_id -> go_toward target_id actor_id) (target_ids state)
          target_ids state = map actor_id $ filter (\target -> 
-}

go_toward :: ActorID -> ActorID -> Task Command State
go_toward actor_id target_id = Task "go toward" [] [update_pos]
    where new_pos state = query_actor target_id Actor.get_pos state
          update_pos state = adjust_actor actor_id (Actor.set_pos (new_pos state)) state

