
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
, find_actors_with
, find_actors_with_inv
, generate
, query_actor
, update_actor
, update_actors
) where

import qualified Data.Map as Map
import Data.Map(Map(..))
import Data.Maybe

type ActorID = Int
type ThingID = String
type Pos = Int

type State = (Map ActorID Actor)
data Actor = Actor ActorID Pos [(Goal, Mission)] (Map ThingID Int) -- ID, position, intents, inventory
data Goal = Goal (State -> Bool) (Maybe (State -> Bool)) (State -> [Mission]) -- success, failure, missions_generator
data Mission = Mission (Maybe (State -> Bool)) [Goal] (State -> State) -- failure, prerequisites, action
type Intent = (Goal, Mission)

goal_is_succeeding :: Goal -> State -> Bool
goal_is_succeeding (Goal success _ _) state = success state

goal_is_failing :: Goal -> State -> Bool
goal_is_failing (Goal _ failure _) state = case failure of { Just ff -> ff state; Nothing -> False }

mission_is_failing :: Mission -> State -> Bool
mission_is_failing (Mission failure _ _) state = case failure of { Just ff -> ff state; Nothing -> False }

mission_prereqs :: Mission -> [Goal]
mission_prereqs (Mission _ prereqs _) = prereqs

actor_inv :: ThingID -> Actor -> Int
actor_inv tid (Actor _ _ _ inventory) = case Map.lookup tid inventory of
                                           Just amount -> amount
                                           Nothing -> 0

actor_add :: ThingID -> Int -> Actor -> Actor
actor_add tid amount actor = Actor aid pos intents (Map.insert tid total inventory)
    where (Actor aid pos intents inventory) = actor
          total = amount + actor_inv tid actor

actor_sub :: ThingID -> Int -> Actor -> Actor
actor_sub tid amount actor = actor_add tid (-amount) actor

actor_id :: Actor -> ActorID
actor_id (Actor aid _ _ _) = aid

actor_pos :: Actor -> Pos
actor_pos (Actor _ pos _ _) = pos

actor_move :: Pos -> Actor -> Actor
actor_move new_pos (Actor aid _ intents inventory) = Actor aid new_pos intents inventory

actor_add_intent :: Intent -> Actor -> Actor
actor_add_intent new_intent (Actor aid pos intents inventory) = Actor aid pos (new_intent : intents) inventory

update_actor :: Actor -> State -> State
update_actor actor actors = Map.insert aid actor actors
    where Actor aid _ _ _ = actor

update_actors :: [Actor] -> State -> State
update_actors [] state = state
update_actors (actor : actors) state = update_actors actors (update_actor actor state)

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

find_actors_with :: (Actor -> Bool) -> State -> [Actor]
find_actors_with predicate state = filter predicate (Map.elems state)

find_actors_with_inv :: ThingID -> Int -> State -> [Actor]
find_actors_with_inv tid amount state = find_actors_with predicate state
    where predicate actor = actor_inv tid actor >= amount

-- you have an unconditional value but they're asking for a function? use this to generate a stupid function to satisfy their stupid requirement
generate :: a -> (b -> a)
generate a = (\_ -> a)

-- 1) if the actor has no intent, crash
-- 2) intent is (Goal, Mission)
-- 3) this means (Goal (State -> Bool) (Maybe (State -> Bool)) (State -> [Mission]), Mission (Maybe (State -> Bool)) [Goal] (State -> State))
-- 4) if the goal is satisfied, pop it
-- 5) if the goal is failed, pop it
-- 6) if the mission is failed, pop it
-- 7) if any of the mission's prereqs are failing, pick a mission for one of them and push it
-- 8) if any of the mission's prereqs are failing, and there's no missions available, then pop it
-- 9) finally, if none of that happened, run the mission's State -> State function!

-- thus: this function needs to be ready to: pick a new mission, update state. Too complex.
-- splitting it into two instead

pop_completed_intents :: State -> [Intent] -> [Intent]
pop_completed_intents _ [] = []
pop_completed_intents state (intent : intents)
  | goal_is_succeeding goal state = update_intents state intents -- pop
  | goal_is_failing goal state = update_intents state intents -- pop
  | mission_is_failing mission state = update_intents state intents -- pop
  | otherwise = intent : intents -- no change here -- this intent pile is unresolved and ready to run
  where (goal, mission) = intent

create_sub_intents :: State -> [Intent] -> [Intent]
create_sub_intents state [] = error "new intent generation not implemented yet"
create_sub_intents state (intent : intents)
  | null unsatisfied_prereqs = intent : intents -- no change here -- this intent pile is ready to run
  | null prereq_solutions = update_intents state intents -- pop because one of the subgoals is failing and there's no way to fix it
  | otherwise = update_intents state (head prereq_solutions : intent : intents) -- complete a subgoal before completing the main one. FIXME: "head" is the dumbest possible solution here. Needs a strong decision!!
  where (goal, mission) = intent
        unsatisfied_prereqs = filter (\goal -> not $ goal_is_succeeding goal state) (mission_prereqs mission)
        prereq_solutions = concat $ map (\goal -> create_intent state goal) unsatisfied_prereqs

update_intents :: State -> [Intent] -> [Intent]
update_intents state [] = update_intents state [generate_fresh_intent state]
update_intents state intents = create_sub_intents state (pop_completed_intents state intents)

update_actor_intents :: State -> Actor -> Actor
update_actor_intents state (Actor aid pos intents inventory) = Actor aid pos (update_intents state intents) inventory

generate_fresh_intent :: State -> Intent
generate_fresh_intent state = error "generating fresh intents is beyond me at this stage"

create_intent :: State -> Goal -> [Intent]
create_intent state goal = map (\mission -> (goal, mission)) missions
    where Goal _ _ missions_generator = goal
          missions = missions_generator state
  
-- we're assuming this is run immediately after update_intents was applied to this Actor
actor_run_intent :: Actor -> State -> State
actor_run_intent actor state = mission_action state
  where Actor _ _ (intent : intents) _ = actor
        (Goal goal_success goal_failure _, Mission mission_failure mission_prereqs mission_action) = intent

-- steps:
-- 1) lookup the actor
-- 2) make sure the actor's Intents are up-to-date and making sense
-- 3) update the state
-- 4) execute the actor's top Intent
run_actor_step :: ActorID -> State -> State
run_actor_step aid state = actor_run_intent new_actor new_state
    where new_state = update_actor new_actor state
          new_actor = update_actor_intents state actor
          actor = find_actor aid state
