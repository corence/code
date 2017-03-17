
module TheSims
(
) where

import Goals
import qualified Data.Map as Map
import Data.Map(Map(..))

hungry :: Actor -> Bool
hungry actor = actor_inv "hunger" actor >= 20

unhungry :: Actor -> Bool
unhungry actor = not $ hungry actor

be_unhungry :: ActorID -> Goal
be_unhungry aid = Goal (query_actor aid unhungry) Nothing (generate $ [eat aid])

eat :: ActorID -> Mission
eat aid = Mission Nothing [have_food aid] consume_food
    where 
          consume_food state = act aid (actor_sub "hunger" 10) $ act aid (actor_sub "food" 1) state

have_food :: ActorID -> Goal
have_food aid = Goal i_has_food (Just no_food_exists) (seek_item "food" aid [aid]) -- TODO: add cooking mission
    where i_has_food = query_actor aid (\actor -> actor_inv "food" actor > 0)
          no_food_exists state = not $ any (\a -> actor_inv "food" a > 0) (Map.elems state)

seek_item :: ThingID -> ActorID -> [ActorID] -> State -> [Mission]
seek_item tid aid blacklist state = take_item_missions
    where dudes_with_item = filter (\actor -> actor_inv tid actor > 0) (Map.elems state)
          dude_ids_with_item = map actor_id dudes_with_item
          take_item_missions = map (\target -> take_item tid target aid) dude_ids_with_item

take_item :: ThingID -> ActorID -> ActorID -> Mission
take_item tid target aid = Mission (Just target_no_longer_has_it) [be_at target aid] exchange
    where target_no_longer_has_it = query_actor target (\actor -> actor_inv tid actor <= 0)
          exchange state = act aid (actor_add tid 1) $ act target (actor_sub tid 1) state

be_at :: ActorID -> ActorID -> Goal
be_at target aid = Goal same_place Nothing (generate [go_to target aid])
    where same_place state = query_actor aid actor_pos state == query_actor target actor_pos state

go_to :: ActorID -> ActorID -> Mission
go_to aid target = Mission Nothing [] update_pos
    where new_pos state = query_actor target actor_pos state
          update_pos state = act aid (actor_move (new_pos state)) state

