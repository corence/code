
module UninformedScan4
( Desire(..)
, Resolution(..)
, PartialResolution(..)
, PartialSolution(..)
, advance_resolutions
, resolve
, start_resolving
) where

import AutoHeap
import Intents

import qualified Queue as Queue
import Queue(Queue(..))

data Desire world = Desire (Goal world) (world -> Float) -- goal, and how good it would be (higher values = more good)
data PartialSolution world = PartialSolution (Intent world) world Float [Intent world]
data Solution world = Solution (Intent world) world Float

advance_single_solution :: PartialSolution world -> [PartialSolution world]
advance_single_solution (PartialSolution _ _ _ []) = []
advance_single_solution ps = case next_intent of
                               (ExecutableIntent _ task) -> map (\stack -> PartialSolution base_intent new_world (base_cost + task_cost task base_world) stack) new_stacks
                               (TaskIntent _ task) -> map (\stack -> PartialSolution base_intent new_world (base_cost + task_cost task base_world) stack) new_stacks -- this is to help debugging / understanding. Should be able to remove it with no change in functionality
                               otherwise -> concat $ map advance_single_solution $ map (\stack -> PartialSolution base_intent new_world base_cost stack) new_stacks
     where (PartialSolution base_intent base_world base_cost (next_intent : base_stack)) = ps
           (new_world, new_stacks) = advance_world base_world (next_intent : base_stack)

-- scans the lowest-cost solution and loops until one of them is actually winning
find_best_solution :: AutoHeap (PartialSolution world) -> Maybe (PartialSolution world)
find_best_solution aheap = do
  first <- AutoHeap.query aheap
  let (PartialSolution f_intent f_world f_cost f_stack) = first in
      if goal_succeeds (intent_goal f_intent) f_world
            then Just first
            else (find_best_solution . advance_solutions) aheap -- remove first, advance_single_solution it, add all the others, then find_best_solution

advance_solutions :: AutoHeap (PartialSolution world) -> AutoHeap (PartialSolution world)
advance_solutions aheap = case AutoHeap.query aheap of
                      Nothing -> aheap
                      Just p -> (\new_ps -> AutoHeap.add_all new_ps ps) (advance_single_solution p)
                      where ps = AutoHeap.remove_head aheap
                      
advance_resolutions :: AutoHeap (PartialResolution world) -> AutoHeap (PartialResolution world)
advance_resolutions aheap = case AutoHeap.query aheap of
                              Nothing -> aheap
                              Just (PartialResolution priority base_solution) -> AutoHeap.add_all new_resolutions other_resolutions
                                  where other_resolutions = AutoHeap.remove_head aheap
                                        new_solutions = advance_single_solution base_solution
                                        new_resolutions = map (PartialResolution priority) new_solutions
                      
-- scans the lowest-cost solution and loops until one of them is actually winning -- but it won't generate any solutions more expensive than max_cost
find_best_solution_capped :: Float -> AutoHeap (PartialSolution world) -> Maybe (PartialSolution world)
find_best_solution_capped max_cost solutions = do
  first <- AutoHeap.query solutions
  let (PartialSolution f_intent f_world f_cost f_stack) = first in
      if goal_succeeds (intent_goal f_intent) f_world
            then Just first
            else if f_cost > max_cost
                 then Nothing
                 else (find_best_solution . advance_solutions) solutions -- remove first, advance_single_solution it, add all the others, then find_best_solution

find_best_resolution :: AutoHeap (PartialResolution world) -> Maybe (Resolution world)
find_best_resolution resolutions = do
  first <- AutoHeap.query resolutions
  let (PartialResolution f_priority (PartialSolution f_intent f_world f_cost f_stack)) = first in
      if goal_succeeds (intent_goal f_intent) f_world
         then Just (Resolution f_priority (Solution f_intent f_world f_cost))
         else (find_best_resolution . advance_resolutions) resolutions -- remove first, advance_single_solution it, add all the others, then find_best_solution

start_resolving :: world -> [Desire world] -> AutoHeap (PartialResolution world)
start_resolving world desires = foldr (\desire aheap -> AutoHeap.add_all (desire_to_partial_resolutions world desire) aheap) (AutoHeap.void pr_comparator) desires

desire_to_partial_resolutions :: world -> Desire world -> [PartialResolution world]
desire_to_partial_resolutions world (Desire goal priority_func) = resolutions
    where solutions = map (\task -> PartialSolution (TaskIntent goal task) world 0 [TaskIntent goal task]) (goal_generate_tasks goal world)
          resolutions = map (PartialResolution (priority_func world)) solutions
          
resolve :: world -> [Desire world] -> Maybe (Resolution world)
resolve world desires = find_best_resolution (start_resolving world desires)

-- resolutions are higher value if their desire is higher, or their cost is lower
data PartialResolution world = PartialResolution Float (PartialSolution world) -- priority, then what we're doing for it
data Resolution world = Resolution Float (Solution world)

ps_comparator :: PartialSolution world -> PartialSolution world -> Ordering
ps_comparator (PartialSolution _ _ cost1 _) (PartialSolution _ _ cost2 _) = compare cost1 cost2

pr_comparator :: PartialResolution world -> PartialResolution world -> Ordering
pr_comparator (PartialResolution priority1 (PartialSolution _ _ cost1 _)) (PartialResolution priority2 (PartialSolution _ _ cost2 _)) = compare (cost1 / priority1 - priority1) (cost2 / priority2 - priority2)
