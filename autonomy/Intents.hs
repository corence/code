
module Intents
( Goal(..)
, Intent(..)
, Task(..)
, intents_extract_actions
, intents_ready
, goal_generate_tasks
, goal_name
, goal_succeeds
, prepare_intents
, task_actions
, task_name
) where

import qualified Data.Map as Map
import Data.Map(Map(..))
import Debug.Trace

--ctrace = trace
ctrace _ = id

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
    | TaskIntent (Goal command state) (Task command state)
    | ExecutableIntent (Goal command state) (Task command state)
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
intents_ready (ExecutableIntent _ _ : _) = True
intents_ready _ = False

intents_extract_actions :: [Intent command state] -> ([Intent command state], [command])
intents_extract_actions [] = ([], [])
intents_extract_actions (intent : other_intents) = case intent of
    ExecutableIntent goal task -> (HazyIntent goal : other_intents, task_actions task)
    otherwise -> (intent : other_intents, [])

prepare_intents :: [Intent command state] -> state -> [Intent command state]
prepare_intents [] _ = ctrace "    time to generate a fresh new intent " $ []
prepare_intents (intent : intents) state = case intent of
    HazyIntent goal -> if goal_succeeds goal state
                       then ctrace ("    " ++ goal_name goal ++ ": popping successful intent ") $ intents
                       else ctrace ("    " ++ goal_name goal ++ ": generating task options: " ++ show (map task_name (goal_generate_tasks goal state))) $ OptionyIntent goal (goal_generate_tasks goal state) : intents
    OptionyIntent goal [] -> ctrace ("    " ++ goal_name goal ++ ": hitting a wall! ") $ []
    OptionyIntent goal tasks -> ctrace ("    " ++ goal_name goal ++ ": winnowing tasks down to " ++ task_name (select_task tasks)) $ TaskIntent goal (select_task tasks) : intents
    TaskIntent goal task -> if null incomplete_prerequisites
                            then ctrace ("    " ++ goal_name goal ++ ": no prerequisites ") $ ExecutableIntent goal task : intents
                            else ctrace ("    " ++ goal_name goal ++ ": preparing prerequisites ") $ HazyIntent (head incomplete_prerequisites) : intent : intents
                            where incomplete_prerequisites = task_incomplete_prerequisites task state
    ExecutableIntent goal _ -> ctrace ("    " ++ goal_name goal ++ ": ready to execute ") $ intent : intents

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

task_incomplete_prerequisites :: Task command state -> state -> [Goal command state]
task_incomplete_prerequisites task state = filter (\goal -> not (goal_succeeds goal state)) (task_prerequisites task)

task_actions :: Task command state -> [command]
task_actions (Task _ _ actions) = actions

select_task :: [Task command state] -> Task command state
select_task [] = error "can't select_task, empty list"
select_task tasks = head tasks
