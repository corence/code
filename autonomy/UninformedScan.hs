{-# OPTIONS -Wall #-}

-- this will generate multiple Intent Stacks, and Lowest-Cost-First search through them until it finds one that satisfies the Goal.

module UninformedScan
(
) where

import Intents

import qualified Heap2 as Heap
import Heap2(Heap(..))

import Debug.Trace

data Option world = Option Float Int world [Intent world]

-- compare 2 options by cost
option_compare :: Option world -> Option world -> Ordering
option_compare (Option cost1 _ _ _) (Option cost2 _ _ _) = Prelude.compare cost1 cost2

scan_step :: Heap (Option world) -> Heap (Option world)
scan_step initial_options = foldr (\intents options -> Heap.add option_compare intents options) (Heap.remove_head option_compare initial_options) (advance_option (Heap.query initial_options))

-- generate all the followup world and intent states, then add the cost of the old and new
advance_option :: Maybe (Option world) -> [Option world]
advance_option Nothing = []
advance_option (Just (Option cost wins world intents)) = map augment new_options
    where augment (Option new_cost new_wins final_world new_stack) = Option (cost + new_cost) (wins + new_wins) final_world new_stack
          new_options = map (\new_stack -> Option (intents_cost world new_stack) 1 new_world new_stack) new_stacks
          (new_world, new_stacks) = advance_world world intents

intents_cost :: world -> [Intent world] -> Float
intents_cost _ [] = 0
intents_cost world (intent : intents) = intent_cost world intent

intent_cost :: world -> Intent world -> Float
intent_cost world (ExecutableIntent _ task) = task_cost task world
intent_cost _ _ = 0

scan :: world -> Goal world -> Maybe (Option world)
scan world goal = full_scan (Heap.fromList option_compare [Option 0 0 world [HazyIntent goal]])

full_scan :: Heap (Option world) -> Maybe (Option world)
full_scan options
  = if Heap.size options <= 1
      then Heap.query options
      else (full_scan . scan_step) options
