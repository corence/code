
module Intents
( Goal(..)
, Intent
, Task(..)
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
type Intent command state = (Goal command state, Maybe [Task command state])

data Goal command state = Goal String [state -> [Task command state]] [state -> Bool] -- name, task_generators, win conditions (need all for success)
data Task command state = Task String [Goal command state] [command] -- name, prerequisites, actions

type ActorID = Int

type Actors command state = Map ActorID ([Intent command state])

step :: ActorID -> Actors command state -> state -> (Actors command state, [command])
step actor_id actors state = (Map.insert actor_id new_intents actors, commands)
    where (new_intents, commands) = step_intents intents state
          intents = case Map.lookup actor_id actors of
                        Just intents -> intents
                        Nothing -> error $ "actor_id not found: " ++ show actor_id

step_intents :: [Intent command state] -> state -> ([Intent command state], [command])
step_intents intents state
    = case prepare_intents intents state of
          (True, new_intents) -> step_intents new_intents state
          otherwise -> let (i : is) = intents in
                          (is, execute_intent i)
          

-- many assumptions are made here. Be prepared before calling this!
execute_intent :: Intent command state -> [command]
execute_intent (_, Just (task : _)) = actions
    where Task _ _ actions = task

prepare_intents :: [Intent command state] -> state -> (Bool, [Intent command state])
prepare_intents [] _ = error "time to generate a fresh new intent"
prepare_intents (intent : intents) state = let (goal, tasks) = intent in
        if goal_succeeds goal state
            then trace "prepare_intents 1 -- popping successful intent" $ (True, intents)
            else case tasks of
                   Nothing -> trace "prepare_intents 2 -- generating options" $ (True, (goal, Just (goal_generate_tasks goal state)) : intents)
                   Just [] -> trace "prepare_intents 3 -- dead end! clearing the whole damn stack" $ (True, [])
                   Just (task : []) -> trace "prepare_intents 4 -- all good, ready to execute" $ (False, intent : intents)
                   Just many_tasks -> trace "prepare_intents 5 -- generating options" $ (True, (goal, Just [select_task many_tasks]) : intents)

goal_succeeds :: Goal command state -> state -> Bool
goal_succeeds (Goal _ _ win_conditions) state = all (\condition -> condition state) win_conditions

goal_generate_tasks :: Goal command state -> state -> [Task command state]
goal_generate_tasks (Goal _ task_generators _) state = concat $ map (\generate_tasks -> generate_tasks state) task_generators

select_task :: [Task command state] -> Task command state
select_task [] = error "can't select_task, empty list"
select_task tasks = head tasks
