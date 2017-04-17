
module UninformedScan2
(
) where

import AutoHeap
import Intents
import Data.Time.Clock

import qualified Queue as Queue
import Queue(Queue(..))

data Desire world = Desire (Goal world) (world -> Float)
data Solution world = Solution (Intent world) Float -- what to do, what it'll cost
data PartialSolution world = PartialSolution (Intent world) Float world [Intent world] -- what to do, what it'll cost (so far), world after doing it, intents to be costed
data SolutionScan world = SolutionScan (AutoHeap (Solution world)) (AutoHeap (PartialSolution world)) (Queue (PartialSolution world)) -- solved, unsolved, rejected
data PathSearches world = PathSearches (Queue (SolutionScan world)) (Queue (SolutionScan world)) (Queue (SolutionScan world)) -- solved, unsolved, rejected

-- Given a goal, this will tell us the first task we should do
scan0 :: Goal world -> Intent world
scan0 _ = error "scan0"

-- Given a goal, this will generate a Solution so we know the cost of it -- and the first Task to be chosen
scan1 :: Goal world -> world -> Solution world
scan1 goal world = scan1a scanner
    where scanner = SolutionScan AutoHeap.void (AutoHeap.fromList partials) Queue.void
          partials = map (\task -> PartialSolution (TaskIntent goal)) tasks
          tasks = map (\task_generator -> task_generator world) task_generators
          (Goal _ task_generators win_conditions) = goal

scan1a :: SolutionScan world -> SolutionScan world
scan1a scan
  | is_empty unsolved = scan
  | 
  where (SolutionScan solved unsolved rejected) = scan

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
