
import Goals
import TheSims
import qualified Data.Map as Map
import Data.Map(Map(..))

state0 = Map.fromList [
   (1, Actor 1 1 [(be_unhungry 1, eat 1)] (Map.fromList [("food", 5)]))
   ]

state1 = Map.fromList [
   (1, Actor 1 1 [(be_unhungry 1, eat 1)] (Map.fromList []))
   ]

state2 = Map.fromList [
   (1, Actor 1 1 [(be_unhungry 1, eat 1)] (Map.fromList [])),
   (2, Actor 2 2 [] (Map.fromList [("oven", 1)])),
   (3, Actor 3 3 [] (Map.fromList [("prepped_ingredients", 1)]))
   ]

main = do
    putStrLn $ show $ run_actor_step 1 state0
    --putStrLn $ show $ run_actor_step 1 state1
    --putStrLn $ show $ run_actor_step 1 state2
    
