
module Goals
( Goal(..)
, Mission(..)
) where

import qualified Data.Map as Map
import Data.Map(Map(..))

type ActorID = Int
type ThingID = String
data GoalStatus = Complete | Failed | Indeterminate
data State = State [Actor]
data Actor = Actor ActorID [Goal] [Mission] (Map ThingID Int)
data Goal = Goal (Actor -> State -> GoalStatus) [(Actor -> State -> Mission)]
data Mission = Mission [Goal] (Actor -> State -> State)

actor_inv :: ThingID -> Actor -> Int
actor_inv tid (Actor _ _ _ inventory) = case Map.lookup tid inventory of
                                           Just amount -> amount
                                           Nothing -> 0

update_actor :: Actor -> State -> State
update_actor actor (State actors)
  | null actors = error $ "can't find actor " ++ show aid
  | sid == aid = State (actor : as)
  | otherwise = update_actor actor (State as)
  where a : as = actors
        Actor aid _ _ _ = actor
        Actor sid _ _ _ = a

actor_add :: ThingID -> Int -> Actor -> Actor
actor_add tid amount actor = Actor aid goals missions (Map.insert tid total inventory)
    where (Actor aid goals missions inventory) = actor
          total = amount + actor_inv tid actor

hungry :: Actor -> Bool
hungry actor = actor_inv "hunger" actor >= 20

unhungry :: Actor -> Bool
unhungry actor = not $ hungry actor

be_unhungry :: Goal
be_unhungry = Goal (\actor _ -> if hungry actor then Indeterminate else Complete) [(\actor state -> eat)]

eat :: Mission
eat = Mission [have_food 1] (\actor -> update_actor (actor_add "hunger" (-10) (actor_add "food" (-1) actor)))

have_food :: Int -> Goal
have_food amount = Goal (\actor _ -> if actor_inv "food" actor >= amount then Complete else Indeterminate) []

{-
have_thing :: ThingID -> Int -> Goal
have_thing tid amount = Goal (\actor _ -> actor_inv tid actor >= amount) [seek tid]

seek :: ThingID -> Actor -> State -> Mission
seek tid actor state = Mission 

get_thing :: ThingID -> State -> [Actor] -> Mission
get_thing tid state target_blacklist = get_thing_from target_id tid

get_thing_from :: ActorID -> ThingID -> Mission
get_thing_from target_id tid = Mission [[be_at target_id, actor_inv target_id tid > 1] (\action state -> take tid actor target state)
    where take tid actor target state = update_actor new_actor (update_actor new_target state)
        where new_actor = actor_add tid 1 actor
              new_target = actor_add tid (-1) target
              target = 

thing_exists :: ThingID -> State -> [Actor] -> Goal
thing_exists tid state actor_blacklist = Goal 
-}
