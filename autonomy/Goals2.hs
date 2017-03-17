
module Goals2
( Goal(..)
, Mission(..)
) where

import qualified Data.Map as Map
import Data.Map(Map(..))

type ActorID = Int
type ThingID = String
data GoalStatus = Complete | Failed | Indeterminate
type Pos = Int

type State = (Map ActorID Actor)
data Actor = Actor ActorID Pos [(Goal, Mission)] (Map ThingID Int) -- ID, position, intentions, inventory
data Goal = Goal (State -> Bool) (Maybe (State -> Bool)) (State -> [Mission]) -- success, failure, missions_generator
data Mission = Mission (Maybe (State -> Bool)) [Goal] (State -> State) -- failure, prerequisites, action

actor_inv :: ThingID -> Actor -> Int
actor_inv tid (Actor _ _ _ inventory) = case Map.lookup tid inventory of
                                           Just amount -> amount
                                           Nothing -> 0

actor_add :: ThingID -> Int -> Actor -> Actor
actor_add tid amount actor = Actor aid pos intentions (Map.insert tid total inventory)
    where (Actor aid pos intentions inventory) = actor
          total = amount + actor_inv tid actor

actor_sub :: ThingID -> Int -> Actor -> Actor
actor_sub tid amount actor = actor_add tid (-amount) actor

actor_id :: Actor -> ActorID
actor_id (Actor aid _ _ _) = aid

actor_pos :: Actor -> Pos
actor_pos (Actor _ pos _ _) = pos

actor_move :: Pos -> Actor -> Actor
actor_move new_pos (Actor aid _ intentions inventory) = Actor aid new_pos intentions inventory

update_actor :: Actor -> State -> State
update_actor actor actors = Map.insert aid actor actors
    where Actor aid _ _ _ = actor

hungry :: Actor -> Bool
hungry actor = actor_inv "hunger" actor >= 20

unhungry :: Actor -> Bool
unhungry actor = not $ hungry actor

-- converts an (Actor -> Bool) function to a (State -> Bool) function
query_actor :: ActorID -> (Actor -> a) -> (State -> a)
query_actor aid function = state_query
    where state_query state = function (find_actor aid state)

-- find the actor, take action upon her, and return her to the state. Nice
act :: ActorID -> (Actor -> Actor) -> (State -> State)
act aid function = state_action
    where state_action state = update_actor (function (find_actor aid state)) state

find_actor :: ActorID -> State -> Actor
find_actor aid state = case Map.lookup aid state of
                           Just actor -> actor
                           Nothing -> error $ "couldn't find actor " ++ show aid

failure_mission :: Mission
failure_mission = Mission (Just (generate True)) [] (generate $ error "you can't execute a failure_mission")

be_unhungry :: ActorID -> Goal
be_unhungry aid = Goal (query_actor aid unhungry) Nothing (generate $ [eat aid])

-- you have an unconditional value but they're asking for a function? use this to generate a stupid function to satisfy their stupid requirement
generate :: a -> (b -> a)
generate a = (\_ -> a)

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
