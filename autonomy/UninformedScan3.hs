
module UninformedScan3
(
) where

import AutoHeap
import Intents
import Data.Time.Clock

import qualified Queue as Queue
import Queue(Queue(..))

data Desire world = Desire (Goal world) (world -> Float)
data Solution world = Solution (Goal world) (Task world) Float -- what we want done, what to do, what it'll cost
--data PartialSolution world = PartialSolution (Goal world) (Task world) Float world (AutoHeap (PartialSolution world)) -- what we want done, what to do, what it'll cost (so far), world after doing it, intents to be costed
data SolutionScan world = SolutionScan (AutoHeap (Solution world)) (AutoHeap (PartialSolution world)) (Queue (PartialSolution world)) -- solved, unsolved, rejected
data PathSearches world = PathSearches (Queue (SolutionScan world)) (Queue (SolutionScan world)) (Queue (SolutionScan world)) -- solved, unsolved, rejected
data TaskSolver world = TaskSolver (Task world) world Float
data GoalSolver world = GoalSolver 

-- 0) if the goal is sated, then do nothing
-- 1) if any prereqs aren't complete, then we need to get those done by making a set of partial solutions
-- 2) if they're all complete, then run the task
-- no options are made 

-- Given a goal, this will generate some Solutions so we know the cost of it -- and the first Task to be chosen
scan1 :: Goal world -> world -> AutoHeap (Solution world)
scan1 goal world = if goal_succeeds goal world
                       then AutoHeap.from_list [Solution goal empty_task 0]
                       else map solve tasks
    where solve task = so
          tasks = map (\task_generator -> task_generator world) task_generators
          (Goal _ task_generators win_conditions) = goal
{-
cost_task :: Task world -> world -> Float -> (world, Float)
cost_task task world cost_so_far = case unsolved_prerequisites of
                                       [] -> cost_so_far + task_cost task world
                                       (u:us) -> cost_task task new_world new_cost
                                       where (new_world, new_cost) = cost_goal u

cost_goal :: Goal world -> world -> Float -> (world, Float)
cost_goal goal world cost_so_far = if goal_succeeds goal world
                                   then (world, cost_so_far)
                                   else 
                                       -- foreach generated task
                                       -- cost it
                                       -- put them all in a Heap
                                       -- return the first
                                       -- OR return nothing if they all failed
                                       
-- solver needs to know:
-- state of the world
-- cost so far
-- the goal

-- each returned thing should be:
-- new world state
-- which task
-- new cost
inspect_goal :: Goal world -> world -> Float -> Maybe (world, Float)
inspect_goal goal world cost_so_far = if goal_succeeds goal world
                                      then Nothing -- but a Good Nothing
                                      else augment (pick_best_task (AutoHeap bomp (goal_generate_tasks goal world))
                                      where augment (task_world, task_cost) = (task_world, cost_so_far + task_cost)
-}

data TS world = TS (Task world) world Float

find_best_task :: Goal world -> AutoHeap (Task world) -> world -> Maybe (world, Float)
find_best_task goal tasks world
  | AutoHeap.is_empty tasks = Nothing
  | goal_succeeds goal world = task
  | otherwise = find_best_task goal new_tasks new_world
  where new_tasks = AutoHeap.add_all generated_tasks tasks_without_head
        tasks_without_head = AutoHeap.pop tasks
        task = AutoHeap.query tasks
        generated_tasks = goal_generate_tasks goal new_world

advance_task :: TS world -> [TS world]
advance_task ts = case do_prerequisites 
    where (TS task world init_cost) = ts
    
do_prerequisites :: TS world -> [Goal world] -> Maybe (TS world)
do_prerequisites ts [] = Just ts
do_prerequisites ts (p : prereqs) = if goal_succeeds goal init_world
                                    then do_prerequisites ts prereqs
                                    else do_prerequisites (TS task next_world (init_cost + next_cost)) (p : prereqs)
                                    where (TS task init_world init_cost) = ts
                                          (next_world, next_cost) = cost_goal p init_world

{-
data Goal world = Goal String [world -> [Task world]] [world -> Bool] -- name, task_generators, win conditions (need all for success)
data Task world = Task String [Goal world] [Command world] (world -> Float) -- name, prerequisites, actions, cost of the actions

    = HazyIntent (Goal world)
    | OptionyIntent (Goal world) [Task world]
    | TaskIntent (Goal world) (Task world)
    | ExecutableIntent (Goal world) (Task world)
    -}

-- Given a bunch of unsolved Desires, this will pick off the biggest desire and solve it.
-- Then for each other Desire, it'll see if it can find a solution, capped by:
--   max cost of Desire 2 = (cost of Desire 1) * (priority of Desire 2) / (priority of Desire 1)
-- So for example -- if Desire 2 is only 1/4 of the priority of Desire 1, then, it is only good if it costs 1/4 or less
-- This will terminate once all candidates have been solved or rejected.
scan2 :: SolutionScan world -> SolutionScan world
scan2 _ = error "scan2"

-- Given a bunch of unsolved Desires from a single Actor, pick the best one -- BUT limit the number of goal expansions that can happen.
-- This is like scan2 with the addition of a limit.
-- The scan can be resumed at a later time.
-- This will terminate when there are no more unsolved -- or when the max number of expansions has been made.
scan3 :: Int -> SolutionScan world -> SolutionScan world
scan3 _ _ = error "scan3"

-- Given a bunch of unsolved Desires from a single Actor, pick the best one -- BUT time-limit it.
-- This is like scan2 with the addition of a time limit.
-- The scan can be resumed at a later time.
-- This will terminate when there are no more unsolved -- or when the time expires.
scan4 :: DiffTime -> SolutionScan world -> IO (SolutionScan world)
scan4 _ _ = error "scan4"

-- Given a whole bunch of SolutionScan, try to solve as many as possible within the expansion limit.
-- Unsolved actors' PathSearches will be queued so that everyone gets a turn.
-- The scan can be resumed at a later time.
-- This will terminate when there are no more unsolved -- or when the max number of expansions has been made.
scan5 :: Int -> PathSearches world -> PathSearches world
scan5 _ _ = error "scan5"

-- Given a whole bunch of SolutionScan, try to solve as many as possible within the time limit.
-- Unsolved actors' PathSearches will be queued so that everyone gets a turn.
-- The scan can be resumed at a later time.
-- This will terminate when there are no more unsolved -- or when the time expires.
scan6 :: DiffTime -> PathSearches world -> IO (PathSearches world)
scan6 _ _ = error "scan6"

