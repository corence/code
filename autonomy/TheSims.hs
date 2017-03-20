
module TheSims
( be_unhungry
, eat
) where

import Goals
import qualified Data.Map as Map
import Data.Map(Map(..))

do_goal :: Goal -> Mission
do_goal goal = Mission Nothing [goal] (error "bad goal for do_goal. it should have already won before trying to execute")

-- take multiple functions that convert input to a list, and return a single function that converts that input to a conjoined list
multi :: [a -> [b]] -> (a -> [b])
multi generators = (\input -> concat $ map (\generator -> generator input) generators)

hungry :: Actor -> Bool
hungry dude = actor_inv "hunger" dude >= 20

unhungry :: Actor -> Bool
unhungry dude = not $ hungry dude

be_unhungry :: ActorID -> Goal
be_unhungry dude_id = Goal (query_actor dude_id unhungry) Nothing (generate $ [eat dude_id])

eat :: ActorID -> Mission
eat dude_id = Mission Nothing [have_food 1 dude_id] consume_food
    where 
          consume_food state = act dude_id (actor_sub "hunger" 10) $ act dude_id (actor_sub "food" 1) state

{-
have_food :: ActorID -> Goal
have_food dude_id = Goal dude_has_food (Just no_food_exists) (multi [cook, seek_item "food" dude_id [dude_id]])
    where dude_has_food = query_actor dude_id (\dude -> actor_inv "food" dude > 0)
          no_food_exists state = not $ any (\a -> actor_inv "food" a > 0) (Map.elems state)
          -}
          
have_food :: Int -> ActorID -> Goal
have_food amount dude_id = have_item "food" amount [generate_cook_missions dude_id] dude_id
          
have_item :: ThingID -> Int -> [State -> [Mission]] -> ActorID -> Goal
have_item tid amount mission_generators dude_id = Goal dude_has_thing (Just no_thing_exists) (multi (seek_item tid dude_id [dude_id] : mission_generators))
    where dude_has_thing state = query_actor dude_id (\dude -> actor_inv tid dude >= amount) state
          no_thing_exists state = sum (map (\actor -> actor_inv tid actor) (Map.elems state)) < amount

-- 2 ways we can do this:
-- 1) foreach oven, generate a mission to cook food in it
-- 2) generate one mission that will constantly scan for ovens and move closer to them
-- we picked the first one
generate_cook_missions :: ActorID -> State -> [Mission]
generate_cook_missions dude_id state = map make_mission oven_ids
    where oven_ids = map actor_id (find_actors_with_inv "oven" 1 state)
          make_mission oven_id = Mission (Just $ failure_conditions oven_id) [have_item "prepped_ingredients" 1 [] dude_id, be_at oven_id dude_id] (make_food oven_id)
          failure_conditions oven_id state = oven_no_longer_exists oven_id state &&  dude_doesnt_have_item state
          dude_doesnt_have_item state = actor_inv "prepped_ingredients" (find_actor dude_id state) <= 0
          oven_no_longer_exists oven_id state = False -- TODO
          -- make_food: decrement the dude's prepped ingredients; increment the oven's food
          make_food oven_id state = update_actors [actor_sub "prepped_ingredients" 1 (find_actor dude_id state), actor_add "food" 1 (find_actor oven_id state)] state

seek_item :: ThingID -> ActorID -> [ActorID] -> State -> [Mission]
seek_item tid dude_id blacklist state = take_item_missions
    where actors_with_item = filter (\target -> actor_inv tid target > 0) (Map.elems state)
          aids_with_item = map actor_id actors_with_item
          take_item_missions = map (\target -> take_item tid target dude_id) aids_with_item

take_item :: ThingID -> ActorID -> ActorID -> Mission
take_item tid target_id dude_id = Mission (Just target_no_longer_has_it) [be_at target_id dude_id] exchange
    where target_no_longer_has_it = query_actor target_id (\target -> actor_inv tid target <= 0)
          exchange state = act dude_id (actor_add tid 1) $ act target_id (actor_sub tid 1) state

be_at :: ActorID -> ActorID -> Goal
be_at target dude_id = Goal same_place Nothing (generate [go_to target dude_id])
    where same_place state = query_actor dude_id actor_pos state == query_actor target actor_pos state

{-
be_at_actor_with :: ThingID -> Int -> ActorID -> Goal
be_at_actor_with tid amount dude_id = Goal any_matches Nothing missions
    where any_matches state = let dude = find_actor dude_id state in any (\target_id -> dude_matches dude (find_actor target_id state))
          dude_matches dude target = actor_pos dude == actor_pos target && actor_inv tid target >= amount && actor_id target /= actor_id dude
          missions state = map (\target_id -> go_to target_id dude_id) (target_ids state)
          target_ids state = map actor_id $ filter (\target -> 
-}

go_to :: ActorID -> ActorID -> Mission
go_to dude_id target = Mission Nothing [] update_pos
    where new_pos state = query_actor target actor_pos state
          update_pos state = act dude_id (actor_move (new_pos state)) state

