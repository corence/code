{-# OPTIONS -Wall #-}

module Costs
( GoalWithCost
, TaskWithCost
, goal_full_cost
, task_full_cost
, measure_goal
, measure_task
) where

import qualified Heap2 as Heap
import Heap2(Heap(..))
--import qualified Data.Map as Map
--import Data.Map(Map)
import Intents
import Debug.Trace

data GoalWithCost command state = SolvedGoal (Goal command state) | GoalWithCost (Goal command state) (Heap (TaskWithCost command state)) -- goal, tasks sorted by cost
data TaskWithCost command state = TaskWithCost (Task command state) state Float -- task, outcome, full cost

goal_full_cost :: GoalWithCost command state -> state -> (state, Float)
goal_full_cost (SolvedGoal _) state = (state, 0)
goal_full_cost (GoalWithCost _ heap) state = trace ("costing goal") $
                                  case Heap.query heap of
                                  Just task_with_cost -> task_full_cost task_with_cost
                                  Nothing -> error "this goal is a failure"

task_full_cost :: TaskWithCost command state -> (state, Float)
task_full_cost (TaskWithCost _ state cost) = (state, cost)

measure_goal :: state -> Goal command state -> GoalWithCost command state
measure_goal state goal
  = trace ("measure_goal " ++ goal_name goal) $ if goal_succeeds goal state
    then SolvedGoal goal
    else GoalWithCost goal curated_tasks
        where curated_tasks = Heap.fromList compare_tasks tasks_with_costs
              tasks_with_costs = map (measure_task state) (goal_generate_tasks goal state)

measure_task :: state -> Task command state -> TaskWithCost command state
measure_task state task = trace ("measure_task " ++ task_name task) $ TaskWithCost task final_state ((sum prereq_costs) + commands_cost)
    where prereq_costs = map (goal_full_cost . measure_goal state) (task_prerequisites task)
          --(new_state, prereqs_cost) = foldr (\goal (state, cost) -> --      map (goal_full_cost . measure_goal state) (task_prerequisites task)
          (ready_state, prereqs_cost) = foldr (\goal (state, cost) -> (goal_full_cost . measure_goal) state) (state, 0) (task_prerequisites task)
          commands_cost = task_cost task state
          final_state = foldr (\action state -> action state) state (task_actions task)

compare_tasks :: TaskWithCost command state -> TaskWithCost command state -> Ordering
compare_tasks left right = Prelude.compare (task_full_cost left) (task_full_cost right)
