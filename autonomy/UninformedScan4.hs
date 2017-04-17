
module UninformedScan4
(
) where

import AutoHeap
import Intents

import qualified Queue as Queue
import Queue(Queue(..))

data Desire world = Desire (Goal world) (world -> Float)
data PartialSolution world = PartialSolution (Intent world) world Float [Intent world]

advance_single_solution :: PartialSolution world -> [PartialSolution world]
advance_single_solution (PartialSolution _ _ _ []) = []
advance_single_solution ps = case next_intent of
           (ExecutableIntent _ task) -> map (\stack -> PartialSolution base_intent new_world (base_cost + task_cost task base_world) stack) new_stacks
           otherwise -> concat $ map (\stack -> advance_single_solution (PartialSolution base_intent new_world base_cost stack)) new_stacks
     where (PartialSolution base_intent base_world base_cost (next_intent : base_stack)) = ps
           (new_world, new_stacks) = advance_world base_world (next_intent : base_stack)

find_best_solution :: AutoHeap (PartialSolution world) -> Maybe (PartialSolution world)
find_best_solution aheap = case AutoHeap.query aheap of
              Nothing -> Nothing
              Just first -> if goal_succeeds (intent_goal f_intent) f_world
                            then Just first
                            else (find_best_solution . advance_solutions) aheap -- remove first, advance_single_solution it, add all the others, then find_best_solution
                            where (PartialSolution f_intent f_world f_cost f_stack) = first

advance_solutions :: AutoHeap (PartialSolution world) -> AutoHeap (PartialSolution world)
advance_solutions aheap = case AutoHeap.query aheap of
                      Nothing -> aheap
                      Just p -> (\new_ps -> AutoHeap.add_all new_ps ps) (advance_single_solution p)
                      where ps = AutoHeap.remove_head aheap
