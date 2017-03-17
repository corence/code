
module Goals
( Actor(..)
, ActorID
, Goal(..)
, Mission(..)
, Pos
, State
, ThingID
, act
, actor_add
, actor_id
, actor_inv
, actor_move
, actor_pos
, actor_sub
, find_actor
, generate
, query_actor
, update_actor
) where

import qualified Data.Map as Map
import Data.Map(Map(..))

type ActorID = Int
type ThingID = String
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

-- you have an unconditional value but they're asking for a function? use this to generate a stupid function to satisfy their stupid requirement
generate :: a -> (b -> a)
generate a = (\_ -> a)

