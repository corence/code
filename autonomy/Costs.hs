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

data GoalWithCost world = SolvedGoal (Goal world) | GoalWithCost (Goal world) (Heap (TaskWithCost world)) -- goal, tasks sorted by cost
data TaskWithCost world = TaskWithCost (Task world) world Float -- task, outcome, full cost

goal_full_cost :: GoalWithCost world -> world -> (world, Float)
goal_full_cost (SolvedGoal _) world = (world, 0)
goal_full_cost (GoalWithCost _ heap) world = trace ("costing goal") $
                                  case Heap.query heap of
                                  Just task_with_cost -> task_full_cost task_with_cost
                                  Nothing -> error "this goal is a failure"

task_full_cost :: TaskWithCost world -> (world, Float)
task_full_cost (TaskWithCost _ world cost) = (world, cost)

measure_goal :: world -> Goal world -> GoalWithCost world
measure_goal world goal
  = trace ("measure_goal " ++ goal_name goal) $ if goal_succeeds goal world
    then SolvedGoal goal
    else GoalWithCost goal curated_tasks
        where curated_tasks = Heap.fromList compare_tasks tasks_with_costs
              tasks_with_costs = map (measure_task world) (goal_generate_tasks goal world)

measure_task :: world -> Task world -> TaskWithCost world
measure_task world task = trace ("measure_task " ++ task_name task) $ TaskWithCost task final_world ((sum prereq_costs) + commands_cost)
    where prereq_costs = map (goal_full_cost . measure_goal world) (task_prerequisites task)
          --(new_world, prereqs_cost) = foldr (\goal (world, cost) -> --      map (goal_full_cost . measure_goal world) (task_prerequisites task)
          (ready_world, prereqs_cost) = foldr (\goal (world, cost) -> (goal_full_cost . measure_goal) world) (world, 0) (task_prerequisites task)
          commands_cost = task_cost task world
          final_world = foldr (\action world -> action world) world (task_actions task)
          
compare_as :: Ord b => (a -> b) -> a -> a -> Ordering
compare_as converter left right = Prelude.compare (converter left) (converter right)

compare_tasks :: TaskWithCost world -> TaskWithCost world -> Ordering
--compare_tasks left right = Prelude.compare (snd $ task_full_cost left) (snd $ task_full_cost right)
compare_tasks = compare_as (\task -> (snd . task_full_cost) task)
