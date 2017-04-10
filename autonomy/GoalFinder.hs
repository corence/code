
module GoalFinder
(
) where

import Intents

-- each actor has a list of Desires: from a state, a Desire can generate a priority value and a goal
type Priority state = state -> Float
data Desire command state = Desire (Priority state) (Goal command state)
data GoalOption command state = GoalOption (Priority state) [Intent command state]
type GoalOptions command state = [(Float, GoalOption command state)] -- todo: make this a Heap

desire_priority :: Desire command state -> Priority state
desire_priority (Desire priority _) = priority

desire_goal :: Desire command state -> Goal command state
desire_goal (Desire _ goal) = goal

add_desire :: Desire command state -> state -> GoalOptions command state -> GoalOptions command state
add_desire desire state options = ((desire_priority desire) state, new_option) : options
    where new_option = GoalOption (desire_priority desire) [HazyIntent (desire_goal desire)]

