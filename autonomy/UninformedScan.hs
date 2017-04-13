
-- this will generate multiple Intent Stacks, and Lowest-Cost-First search through them until it finds one that satisfies the Goal.

module UninformedScan
( Option(..)
, scan
, scan_step
) where

import Intents

import qualified Heap2 as Heap
import Heap2(Heap(..))

import Debug.Trace

data Option world = Option Float Int world [Intent world]
data Options world = Options (Heap (Option world)) (Heap (Option world)) -- solved, unsolved

is_solved :: Option world -> Bool
is_solved (Option _ _ _ []) = True
is_solved _ = False

-- compare 2 options by cost
option_compare :: Option world -> Option world -> Ordering
option_compare (Option cost1 _ _ _) (Option cost2 _ _ _) = Prelude.compare cost1 cost2

add_option :: Option world -> Options world -> Options world
add_option option (Options solved unsolved)
  = if is_solved option
      then Options (Heap.add option_compare option solved) unsolved
      else Options solved (Heap.add option_compare option unsolved)

scan_step :: Options world -> Options world
scan_step (Options init_solved_options init_unsolved_options)
  = foldr add_option (Options init_solved_options other_unsolved_options) (advance_option (Heap.query init_unsolved_options))
      where other_unsolved_options = Heap.remove_head option_compare init_unsolved_options

-- generate all the followup world and intent states, then add the cost of the old and new
advance_option :: Maybe (Option world) -> [Option world]
advance_option Nothing = []
advance_option (Just (Option cost wins world intents)) = map augment new_options
    where augment (Option new_cost new_wins final_world new_stack) = Option (cost + new_cost) (wins + new_wins) final_world new_stack
          new_options = map (\new_stack -> Option (intents_cost world new_stack) 1 new_world new_stack) new_stacks
          (new_world, new_stacks) = advance_world world intents

intents_cost :: world -> [Intent world] -> Float
intents_cost _ [] = 0
intents_cost world (intent : _) = intent_cost world intent

intent_cost :: world -> Intent world -> Float
intent_cost world (ExecutableIntent _ task) = task_cost task world
intent_cost _ _ = 0

scan :: world -> Goal world -> Maybe (Option world)
scan world goal = Heap.query solutions
    where initial_options = Options (Heap.void) (Heap.fromList option_compare [Option 0 0 world [HazyIntent goal]])
          Options solutions unsolutions = scan_until_solution initial_options

scan_until_solution :: Options world -> Options world
scan_until_solution options
    = let Options solved unsolved = options in
        if (Heap.size solved >= 1) || (Heap.size unsolved < 1)
          then options
          else (scan_until_solution . scan_step) options
