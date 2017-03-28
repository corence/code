
module Intents
( Goal(..)
, Intent(..)
, Task(..)
, intents_extract_actions
, intents_ready
, goal_name
, prepare_intents
, task_actions
, task_name
) where

import qualified Data.Map as Map
import Data.Map(Map(..))
import Debug.Trace

-- if i have no Intents then i need to generate one
-- if i have at least one intent, then look at the head:
---- does the Goal pass? if so, pop this intent
---- is there Nothing for the task_options? in that case, the Goal needs to generate a bunch of them
---- is there [] for the task_options? if so, CLEAR the whole intent stack (we've reached a failure state and we're at risk of endless looping now)
---- is there (x:[]) for the task_options? perfect; this stack is ready to execute
---- otherwise there are too many task_options; we need to filter some out
data Intent command state
    = HazyIntent (Goal command state)
    | OptionyIntent (Goal command state) [Task command state]
    | ClearIntent (Goal command state) (Task command state)
    deriving Show
data Goal command state = Goal String [state -> [Task command state]] [state -> Bool] -- name, task_generators, win conditions (need all for success)
data Task command state = Task String [Goal command state] [command] -- name, prerequisites, actions

instance Show (Task command state) where
    show (Task name _ _) = "Task " ++ name

instance Show (Goal command state) where
    show (Goal name _ _) = "Goal " ++ name

type ActorID = Int

intents_ready :: [Intent command state] -> Bool
intents_ready [] = False
intents_ready (ClearIntent _ _ : _) = True
intents_ready _ = False

intents_extract_actions :: [Intent command state] -> ([Intent command state], [command])
intents_extract_actions [] = ([], [])
intents_extract_actions (intent : other_intents) = case intent of
    HazyIntent _ -> (intent : other_intents, [])
    OptionyIntent _ _ -> (intent : other_intents, [])
    ClearIntent goal task -> (HazyIntent goal : other_intents, task_actions task)

-- if it returns True, then we'll need to run it again (because something changed)
prepare_intents :: [Intent command state] -> state -> [Intent command state]
prepare_intents [] _ = trace "    time to generate a fresh new intent " $ []
prepare_intents (intent : intents) state = case intent of
    HazyIntent goal -> if goal_succeeds goal state
                       then trace "    popping successful intent " $ intents
                       else trace "    generating task options " $ OptionyIntent goal (goal_generate_tasks goal state) : intents
    OptionyIntent goal [] -> trace "    hitting a wall! " $ []
    OptionyIntent goal tasks -> trace "    winnowing tasks " $ ClearIntent goal (select_task tasks) : intents
    ClearIntent goal task -> trace "    ready to execute " $ intent : intents

goal_succeeds :: Goal command state -> state -> Bool
goal_succeeds (Goal _ _ win_conditions) state = all (\condition -> condition state) win_conditions

goal_generate_tasks :: Goal command state -> state -> [Task command state]
goal_generate_tasks (Goal _ task_generators _) state = concat $ map (\generate_tasks -> generate_tasks state) task_generators

goal_name :: Goal command state -> String
goal_name (Goal name _ _) = name

task_name :: Task command state -> String
task_name (Task name _ _) = name

task_prerequisites :: Task command state -> [Goal command state]
task_prerequisites (Task _ prerequisites _) = prerequisites

task_actions :: Task command state -> [command]
task_actions (Task _ _ actions) = actions

select_task :: [Task command state] -> Task command state
select_task [] = error "can't select_task, empty list"
select_task tasks = head tasks
