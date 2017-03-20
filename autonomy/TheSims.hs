
module TheSims
( be_unhungry
, eat
) where

import Tasks
import qualified Data.Map as Map
import Data.Map(Map(..))

{-
 - data Task =
 -     TGoal String [State -> [Task]] [State -> Bool] -- name, solution_generators, successes
 -     | TTask String [Task] (State -> State) Bool -- name, prerequisites, action, repeating
 -}

hungry :: Actor -> Bool
hungry dude = actor_inv "hunger" dude >= 20

unhungry :: Actor -> Bool
unhungry dude = not $ hungry dude

be_unhungry :: ActorID -> Task
be_unhungry dude_id = TTask "be_unhungry" [query_actor dude_id unhungry] [eat dude_id]

eat :: ActorID -> Task
eat dude_id = TTask "eat" [have_food 1 dude_id] consume_food False
    where consume_food state = adjust_actor dude_id (actor_sub "hunger" 10) $ adjust_actor dude_id (actor_sub "food" 1) state

have_food :: Int -> ActorID -> Task
have_food amount dude_id = have_item "food" amount [generate_cook_missions dude_id] dude_id
          
have_item :: ThingID -> Int -> [State -> [Task]] -> ActorID -> Task
have_item tid amount task_generators dude_id = TTask "have_item" already_has_item [seek_item tid dude_id [dude_id]] -- todo: generators!
    where already_has_item = (query_actor dude_id (\dude -> actor_inv tid dude >= amount))

-- 2 ways we can do this:
-- 1) foreach oven, generate a mission to cook food in it
-- 2) generate one mission that will constantly scan for ovens and move closer to them
-- we picked the first one
generate_cook_missions :: ActorID -> State -> [Task]
generate_cook_missions dude_id state = map make_mission oven_ids
    where oven_ids = map actor_id (find_actors_with_inv "oven" 1 state)
          make_mission oven_id = TTask "cook in this oven" [have_item "prepped_ingredients" 1 [] dude_id, be_at oven_id dude_id] (make_food oven_id) -- TODO: mission generators
          -- make_food: decrement the dude's prepped ingredients; increment the oven's food
          make_food oven_id state = replace_actors [actor_sub "prepped_ingredients" 1 (find_actor dude_id state), actor_add "food" 1 (find_actor oven_id state)] state

seek_item :: ThingID -> ActorID -> [ActorID] -> State -> [Task]
seek_item tid dude_id blacklist state = take_item_missions
    where actors_with_item = filter (\target -> actor_inv tid target > 0) (Map.elems state)
          aids_with_item = map actor_id actors_with_item
          take_item_missions = map (\target -> take_item tid target dude_id) aids_with_item

take_item :: ThingID -> ActorID -> ActorID -> Task
take_item tid target_id dude_id = TTask name [be_at target_id dude_id] exchange False
    where name = ("take_item " ++ show tid ++ " from " ++ show target_id)
          exchange state = adjust_actor dude_id (actor_add tid 1) $ adjust_actor target_id (actor_sub tid 1) state

be_at :: ActorID -> ActorID -> Task
be_at target_id dude_id = TGoal ("be at " ++ show target_id) [\_ -> [go_to target_id dude_id]] [same_place]
    where same_place state = query_actor dude_id actor_pos state == query_actor target_id actor_pos state

{-
be_at_actor_with :: ThingID -> Int -> ActorID -> Task
be_at_actor_with tid amount dude_id = Task any_matches Nothing missions
    where any_matches state = let dude = find_actor dude_id state in any (\target_id -> dude_matches dude (find_actor target_id state))
          dude_matches dude target = actor_pos dude == actor_pos target && actor_inv tid target >= amount && actor_id target /= actor_id dude
          missions state = map (\target_id -> go_to target_id dude_id) (target_ids state)
          target_ids state = map actor_id $ filter (\target -> 
-}

go_to :: ActorID -> ActorID -> Task
go_to dude_id target = TTask "goto" [] update_pos True
    where new_pos state = query_actor target actor_pos state
          update_pos state = adjust_actor dude_id (actor_move (new_pos state)) state

