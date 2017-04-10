
module GoalFinder
(
) where

import Intents
import qualified Heap as Heap
import Heap(Heap(..))
import qualified Data.Map as Map
import Data.Map(Map(..))

type Preferences command = command

-- each actor has a list of Desires: from a state, a Desire can generate a priority value and a goal
type Priority state = state -> Float
data Desire command state = Desire (Priority state) (Goal command state)
data GoalOption command state = GoalOption (Priority state) [Intent command state]
type GoalOptions command state = Heap Float (GoalOption command state)

desire_priority :: Desire command state -> Priority state
desire_priority (Desire priority _) = priority

desire_goal :: Desire command state -> Goal command state
desire_goal (Desire _ goal) = goal

add_desire :: Desire command state -> state -> GoalOptions command state -> GoalOptions command state
add_desire desire state options = Heap.add (new_desire, new_option) options
    where new_option = GoalOption (desire_priority desire) [HazyIntent (desire_goal desire)]
          new_desire = (desire_priority desire) state

-- for each step we'll expand one of the options
-- 2 factors in assessing a half-expanded option:
--  - how bad do we want it? (the desire priority value from the current State)
--  - how much will it cost us? (the cumulative cost so far in various currencies -- time, money, health -- adjusted by how much the actor likes those currencies
-- Each time we want to expand an option we should 
--  - remove it from the heap
--  - extend its intents
--  - if it's good, mark it as the solution and throw away the other options
--  - if it's junk, throw it away
--  - otherwise, update its cost with our new info
--  - then put it back in the heap


-- simpler thing to explore: given a set of Task objects, which one is the cheapest, for the resources that I care about? (ie: do i cook, walk to pickup food, or walk further to pickup food?)

-- once that is done, we can then say: pick a Desire based on which one i want more and which one has the most expensive Goal
--estimate_task_cost :: Preferences command -> state -> Task command state -> Float
--estimate_task_cost preferences state (Task _ goals commands) = sum (map (estimate_goal_cost preferences state) goals) + sum (map (estimate_command_cost preferences state) commands)

--estimate_goal_cost :: Preferences command -> state -> Goal command state -> Float
--estimate_goal_cost preferences state (Goal _ task_generators win_conditions) = something


-- alternatively: it probably just makes sense to build the solution as we're measuring it.
-- update: no, this solution sucks
{-
type Costs = Map String Float

data TaskSolution command state
    = UnstartedTaskSolution (Task command state)
    | PartialTaskSolution (Task command state) (Costs) [GoalSolution command state]
    | FinalTaskSolution (Task command state) (Costs)

data GoalSolution command state
    = UnstartedGoalSolution (Goal command state)
    | PartialGoalSolution (Goal command state) (Heap (TaskSolution command state)) [TaskSolution command state] -- solved tasks then unsolved tasks
    | FinalGoalSolution (Goal command state) (Costs) (Task command state)
    | SkipGoalSolution (Goal command state)
    | FailGoalSolution (Goal command state)

goal_advance :: state -> GoalSolution command state -> GoalSolution command state
goal_advance state (UnstartedGoalSolution goal)
    = if goal_succeeds goal state
      then SkipGoalSolution goal
      else PartialGoalSolution goal Map.empty (goal_generate_tasks goal state)
goal_advance state (PartialGoalSolution goal costs solved_tasks unsolved_tasks)
    | null unsolved_tasks = if null solved_tasks
                            then FailGoalSolution goal
                            else let task = Heap.query solved_tasks in FinalGoalSolution goal (task_solution_costs task) task
    -- | otherwise = 

task_solution_costs = const
-}
    
    

--solution_advance :: Solution command state -> Solution command state
--solution_advance (Solution True costs goal) = Solution True costs goal
--solution_advance (Solution False costs goal) = 

{-
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
    -}
