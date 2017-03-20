
module Tasks
(
) where

import qualified Data.Map as Map
import Data.Map(Map(..))
import Debug.Trace

type ActorID = Int
type ThingID = String
type Pos = Int

type State = (Map ActorID Actor)
data Actor = Actor ActorID Pos [Task] (Map ThingID Int) -- ID, position, tasks, inventory

data Task = Task String [State -> Bool] [State -> [Task]] (State -> State) -- name, completions, prerequisite_generators, action

instance Show Actor where
    show (Actor aid pos intents inventory) = "Actor #" ++ show aid ++ " @" ++ show pos ++ " intents " ++ show (length intents) ++ " inv " ++ show inventory

task_is_done :: Task -> State -> Bool
task_is_done (Task _ completions _ _) state = any (\is_complete -> is_complete state) completions

generate_prerequisites :: Task -> State -> [Task]
generate_prerequisites (Task _ _ prerequisite_generators _) state = concat $ map (\generate -> generate state) prerequisite_generators

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
adjust_actor :: ActorID -> (Actor -> Actor) -> (State -> State)
adjust_actor aid function = state_action
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

-- this will either adjust the dude's current tasks, or it will run one
-- 0) if no tasks are on the stack, create one
-- 1) if the task is done (pass/fail), pop it (this may trigger #0)
-- 2) if any prerequisite tasks are needed, generate one (this will likely trigger #2)
-- 3) otherwise, run the task

step_tasks :: ActorID -> State -> State
step_tasks dude_id state
  | dude_has_no_tasks = create_task
  | dude_task_is_complete = pop_task
  | null prerequisites = run_task
  | otherwise = add_prerequisite
  where dude = find_actor dude_id state
        Actor _ dude_pos dude_tasks dude_inventory = dude
        task = head dude_tasks
        Task task_name task_completions task_generators task_action = task
        dude_has_no_tasks = null dude_tasks
        dude_task_is_complete = task_is_done task state
        prerequisites = generate_prerequisites task state
        create_task = error "idk how to create a task yet"
        pop_task = adjust_actor dude_id (\_ -> Actor dude_id dude_pos (tail dude_tasks) dude_inventory) state
        run_task = task_action state
        add_prerequisite = adjust_actor dude_id (\_ -> Actor dude_id dude_pos (head prerequisites : dude_tasks) dude_inventory) state
