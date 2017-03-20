
module Tasks
( Actor(..)
, ActorID
, Pos
, State
, Task(..)
, ThingID
, actor_add
, actor_id
, actor_inv
, actor_move
, actor_pos
, actor_sub
, adjust_actor
, find_actor
, find_actors_with
, find_actors_with_inv
, query_actor
, step_tasks
, replace_actor
, replace_actors
) where

import qualified Data.Map as Map
import Data.Map(Map(..))
import Debug.Trace

type ActorID = Int
type ThingID = String
type Pos = Int

type State = (Map ActorID Actor)
data Actor = Actor ActorID Pos [Task] (Map ThingID Int) -- ID, position, tasks, inventory

data Task =
    TGoal String [State -> [Task]] [State -> Bool] -- name, solution_generators, successes
    | TTask String [State -> [Task]] (State -> State) Bool -- name, prerequisite_generators, action, repeating

instance Show Actor where
    show (Actor aid pos intents inventory) = "Actor #" ++ show aid ++ " @" ++ show pos ++ " intents " ++ show (length intents) ++ " inv " ++ show inventory

task_is_done :: Task -> State -> Bool
task_is_done (TGoal _ _ successes) state = any (\success -> success state) successes
task_is_done (TTask _ _ _ _) state = False

generate_solutions :: Task -> State -> [Task]
generate_solutions (TGoal _ solution_generators _) state = concat $ map (\solve -> solve state) solution_generators
generate_solutions (TTask _ _ _ _) _ = []

generate_prerequisites :: Task -> State -> [Task]
generate_prerequisites (TGoal _ _ _) _ = []
generate_prerequisites (TTask _ prerequisite_generators _ _) state = concat $ map (\generate -> generate state) prerequisite_generators

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

replace_actor :: Actor -> State -> State
replace_actor actor actors = Map.insert aid actor actors
    where Actor aid _ _ _ = actor

replace_actors :: [Actor] -> State -> State
replace_actors [] state = state
replace_actors (actor : actors) state = replace_actors actors (replace_actor actor state)

-- converts an (Actor -> Bool) function to a (State -> Bool) function
query_actor :: ActorID -> (Actor -> a) -> (State -> a)
query_actor aid function = state_query
    where state_query state = function (find_actor aid state)

-- find the actor, take action upon her, and return her to the state. Nice
adjust_actor :: ActorID -> (Actor -> Actor) -> (State -> State)
adjust_actor aid function = state_action
    where state_action state = replace_actor (function (find_actor aid state)) state

find_actor :: ActorID -> State -> Actor
find_actor aid state = case Map.lookup aid state of
                           Just actor -> actor
                           Nothing -> error $ "couldn't find actor " ++ show aid

find_actors_with :: (Actor -> Bool) -> State -> [Actor]
find_actors_with predicate state = filter predicate (Map.elems state)

find_actors_with_inv :: ThingID -> Int -> State -> [Actor]
find_actors_with_inv tid amount state = find_actors_with predicate state
    where predicate actor = actor_inv tid actor >= amount

-- this will either adjust the dude's current tasks, or it will run one
step_tasks :: ActorID -> State -> State
step_tasks dude_id state
  | null dude_tasks = trace "step_tasks 1" $ create_task
  | otherwise = case (head dude_tasks) of
                    (TGoal _ _ _) -> step_goal dude_id state
                    (TTask _ _ _ _) -> step_task dude_id state
  where dude = find_actor dude_id state
        Actor _ dude_pos dude_tasks dude_inventory = dude
        create_task = error "idk how to create a task yet"

step_goal :: ActorID -> State -> State
step_goal dude_id state
  | task_is_done task state = trace "step_goal 1" $ dismiss_this_goal
  | null solutions = trace "step_goal 2" $ dismiss_this_goal
  | otherwise = trace "step_goal 3" $ add_solution
  where dude = find_actor dude_id state
        Actor _ dude_pos dude_tasks dude_inventory = dude
        task = head dude_tasks
        solutions = generate_solutions task state
        add_solution = adjust_actor dude_id (\_ -> Actor dude_id dude_pos (head solutions : dude_tasks) dude_inventory) state -- TODO: this should be smarter than head
        dismiss_this_goal = replace_actor (actor_pop_task dude) state

actor_pop_task :: Actor -> Actor
actor_pop_task (Actor aid a_pos a_tasks a_inv) = Actor aid a_pos (tail a_tasks) a_inv

actor_set_tasks :: [Task] -> Actor -> Actor
actor_set_tasks new_tasks (Actor aid a_pos _ a_inv) = Actor aid a_pos new_tasks a_inv

step_task :: ActorID -> State -> State
step_task dude_id state
  | null prerequisites = trace "step_task 1" $ if task_repeating then task_action state else pop_task $ task_action state
  | otherwise = trace "step_task 2" $ add_prerequisite
  where dude = find_actor dude_id state
        Actor _ dude_pos dude_tasks dude_inventory = dude
        task = head dude_tasks
        TTask _ _ task_action task_repeating = task
        prerequisites = generate_prerequisites task state
        add_prerequisite = adjust_actor dude_id (\_ -> Actor dude_id dude_pos (head prerequisites : dude_tasks) dude_inventory) state
        pop_task state = adjust_actor dude_id (\_ -> Actor dude_id dude_pos (tail dude_tasks) dude_inventory) state
