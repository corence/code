
module Intents
( Command(..)
, Goal(..)
, Intent(..)
, Intents(..)
, Task(..)
, advance_intents
, advance_world
, intents_extract_actions
, intent_goal
, intent_task
, intents_ready
, goal_generate_tasks
, goal_name
, goal_succeeds
, task_actions
, task_cost
, task_name
, task_prerequisites
) where

import qualified Data.Map as Map
import Data.Map(Map(..))
import Debug.Trace

import Control.Monad.State

--ctrace = trace
ctrace _ = id

-- if i have no Intents then i need to generate one
-- if i have at least one intent, then look at the head:
---- does the Goal pass? if so, pop this intent
---- is there Nothing for the task_options? in that case, the Goal needs to generate a bunch of them
---- is there [] for the task_options? if so, CLEAR the whole intent stack (we've reached a failure world and we're at risk of endless looping now)
---- is there (x:[]) for the task_options? perfect; this stack is ready to execute
---- otherwise there are too many task_options; we need to filter some out
type Command world = world -> world
data Goal world = Goal String [world -> [Task world]] [world -> Bool] -- name, task_generators, win conditions (need all for success)
data Task world = Task String [Goal world] [Command world] (world -> Float) -- name, prerequisites, actions, cost of the actions
data Intent world
    = HazyIntent (Goal world)
    | OptionyIntent (Goal world) [Task world]
    | TaskIntent (Goal world) (Task world)
    | ExecutableIntent (Goal world) (Task world)
    deriving Show
type Intents world = [Intent world]

instance Show (Task world) where
    show (Task name _ _ _) = "Task " ++ name

instance Show (Goal world) where
    show (Goal name _ _) = "Goal " ++ name

type ActorID = Int

intents_ready :: Intents world -> Bool
intents_ready [] = False
intents_ready (ExecutableIntent _ _ : _) = True
intents_ready _ = False

intents_extract_actions :: Intents world -> (Intents world, [Command world])
intents_extract_actions [] = ([], [])
intents_extract_actions (intent : other_intents) = case intent of
    ExecutableIntent goal task -> (HazyIntent goal : other_intents, task_actions task)
    otherwise -> (intent : other_intents, [])

advance_world :: world -> Intents world -> (world, [Intents world])
advance_world world [] = error "can't advance empty intents"
advance_world world (intent:intents)
  = case intent of
      ExecutableIntent _ task -> (new_world, [intents])
        where new_world = foldr (\action world -> action world) world (task_actions task)
      otherwise -> (world, advance_intents world (intent : intents))

advance_intents :: world -> Intents world -> [Intents world]
advance_intents world [] = ctrace "    time to generate a fresh new intent " $ []
advance_intents world (intent : intents) = case intent of
    HazyIntent goal -> if goal_succeeds goal world
                       then ctrace ("    " ++ goal_name goal ++ ": popping successful intent ") $ [intents]
                       else ctrace ("    " ++ goal_name goal ++ ": generating task options: " ++ show (map task_name (goal_generate_tasks goal world))) $ [OptionyIntent goal (goal_generate_tasks goal world) : intents]
    OptionyIntent goal [] -> ctrace ("    " ++ goal_name goal ++ ": hitting a wall! ") $ []
    OptionyIntent goal tasks -> ctrace ("    " ++ goal_name goal ++ ": blossoming tasks into " ++ show (length tasks) ++ " streams") $ map (: intents) (map (TaskIntent goal) tasks)
    TaskIntent goal task -> if null incomplete_prerequisites
                            then ctrace ("    " ++ goal_name goal ++ ": no prerequisites ") $ [ExecutableIntent goal task : intents]
                            else ctrace ("    " ++ goal_name goal ++ ": preparing prerequisites ") $ [HazyIntent (head incomplete_prerequisites) : intent : intents]
                            where incomplete_prerequisites = task_incomplete_prerequisites task world
    ExecutableIntent goal _ -> ctrace ("    " ++ goal_name goal ++ ": ready to execute ") $ [intent : intents]

intent_goal :: Intent world -> Goal world
intent_goal (HazyIntent goal) = goal
intent_goal (OptionyIntent goal _) = goal
intent_goal (TaskIntent goal _) = goal
intent_goal (ExecutableIntent goal _) = goal

intent_task :: Intent world -> Maybe (Task world)
intent_task (HazyIntent _) = Nothing
intent_task (OptionyIntent _ tasks) = Nothing
intent_task (TaskIntent _ task) = Just task
intent_task (ExecutableIntent _ task) = Just task

goal_succeeds :: Goal world -> world -> Bool
goal_succeeds (Goal _ _ win_conditions) world = all (\condition -> condition world) win_conditions

goal_generate_tasks :: Goal world -> world -> [Task world]
goal_generate_tasks (Goal _ task_generators _) world = concat $ map (\generate_tasks -> generate_tasks world) task_generators

goal_name :: Goal world -> String
goal_name (Goal name _ _) = name

task_name :: Task world -> String
task_name (Task name _ _ _) = name

task_prerequisites :: Task world -> [Goal world]
task_prerequisites (Task _ prerequisites _ _) = prerequisites

task_cost :: Task world -> world -> Float
task_cost (Task _ _ _ cost) = cost

task_incomplete_prerequisites :: Task world -> world -> [Goal world]
task_incomplete_prerequisites task world = filter (\goal -> not (goal_succeeds goal world)) (task_prerequisites task)

task_actions :: Task world -> [Command world]
task_actions (Task _ _ actions _) = actions
