
module Goals2
( Goal(..)
, Mission(..)
) where

import qualified Data.Map as Map
import Data.Map(Map(..))

type ActorID = Int
type ThingID = String
data GoalStatus = Complete | Failed | Indeterminate

type State = (Map ActorID Actor)
data Actor = Actor ActorID [(Goal, Mission)] (Map ThingID Int) -- ID, intentions, inventory
data Goal = Goal (State -> Bool) (Maybe (State -> Bool)) [(State -> Mission)] -- success, failure, mission_generators
data Mission = Mission (State -> Bool) [Goal] (State -> State) -- failure, prerequisites, action

actor_inv :: ThingID -> Actor -> Int
actor_inv tid (Actor _ _ inventory) = case Map.lookup tid inventory of
                                           Just amount -> amount
                                           Nothing -> 0

actor_add :: ThingID -> Int -> Actor -> Actor
actor_add tid amount actor = Actor aid intentions (Map.insert tid total inventory)
    where (Actor aid intentions inventory) = actor
          total = amount + actor_inv tid actor

actor_sub :: ThingID -> Int -> Actor -> Actor
actor_sub tid amount actor = actor_add tid (-amount) actor

update_actor :: Actor -> State -> State
update_actor actor actors = Map.insert aid actor actors
    where Actor aid _ _ = actor

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
    where state_action state = update_actor (function (find_actor state)) state

find_actor :: ActorID -> State -> Actor
find_actor aid state = case Map.lookup aid state of
                           Just actor -> actor
                           Nothing -> error $ "couldn't find actor " ++ show aid

be_unhungry :: ActorID -> Goal
be_unhungry aid = Goal (query_actor aid unhungry) Nothing [find_food aid]

eat :: ActorID -> Mission
eat aid = Mission failure_condition [have_food aid] consume_food
    where failure_condition state = not $ any (\a -> actor_inv "food" a > 0) (Map.elems state)
          consume_food state = act aid (actor_sub "hunger" 10) $ act aid (actor_sub "food" 1) state
