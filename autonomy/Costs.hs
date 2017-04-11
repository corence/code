
module Costs
(
) where

import qualified Heap2 as Heap
import Heap2(Heap(..))
import qualified Data.Map as Map
import Data.Map(Map(..))
import Intents

data GoalWithCost command state = SolvedGoal (Goal command state) | GoalWithCost (Goal command state) (Heap (TaskWithCost command state)) -- goal, tasks sorted by cost
data TaskWithCost command state = TaskWithCost (Task command state) Float -- task, cost

goal_cost :: GoalWithCost command state -> Float
goal_cost (SolvedGoal _) = 0
goal_cost (GoalWithCost _ heap) = case Heap.query heap of
                                  Just task_with_cost -> task_cost task_with_cost
                                  Nothing -> error "this goal is a failure"

task_cost :: TaskWithCost command state -> Float
task_cost (TaskWithCost _ cost) = cost

measure_goal :: state -> Goal command state -> GoalWithCost command state
measure_goal state goal
  = if goal_succeeds goal state
    then SolvedGoal goal
    else GoalWithCost goal curated_tasks
        where curated_tasks = Heap.fromList compare_tasks tasks_with_costs
              tasks_with_costs = map (measure_task state) (goal_generate_tasks goal state)

measure_task :: state -> Task command state -> TaskWithCost command state
measure_task state task = TaskWithCost task ((sum prereq_costs) + sum (command_costs))
    where prereq_costs = map (goal_cost . measure_goal state) (task_prerequisites task)
          command_costs = map (const 1) (task_actions task)

compare_tasks :: TaskWithCost command state -> TaskWithCost command state -> Ordering
compare_tasks left right = Prelude.compare (task_cost left) (task_cost right)
