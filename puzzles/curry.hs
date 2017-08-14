
{-
You are at a very fancy dinner party with some very good friends, trying to eat a very hot curry. On your plate is a pile of curry, rice and vegetables. You are finding it quite difficult to eat, since the curry is too hot, the rice is too sticky and you don't like vegetables.

To work around this problem, you eat in such a way that each mouthful includes a scoop of two different items. For example, the first mouthful might include a scoop of curry with a scoop of rice. The next mouthful might include a scoop of rice with a scoop of vegetables. You cannot eat three scoops all at once, since your spoon is too small.

The danger with your plan is that you might not be able to finish your meal. At last week's dinner party you started off by eating all your curry and rice, which meant that you could not finish your vegetables since there was nothing left to eat them with. It was most embarrassing. Determined not to be humiliated again, you pull your laptop out of your bag and tap away secretly beneath the table, hoping to solve your eating problems for once and for all.

Your task is to work out how to eat your meal so that you eat as much total food as possible.

Input
There will be only one line of input. This line will contain the three integers c r v (each separated by a single space), representing the number of scoops of curry, rice and vegetables on your plate. You are guaranteed that 0 <= c,r,v <= 100,000.

Output
The output must be a single line containing the integers x, y and z (separated by spaces), where:

x is the number of mouthfuls containing a scoop of rice with a scoop of vegetables;
y is the number of mouthfuls containing a scoop of curry with a scoop of vegetables;
z is the number of mouthfuls containing a scoop of curry with a scoop of rice.
You must choose x, y and z so that you eat the maximum total number of scoops possible. If there is more than one way of doing this, any solution will do.
-}

-- input: c r v
-- output: rv cv cr
-- thus
-- input: 100 10 1
-- output: 011 101 110

-- examples
--5 6 9
--5 0 3
--5 1 4 -- 5 * rv

--1 2 5
--1 0 3


import Data.List
import qualified Data.Map as Map
import Data.Map(Map(..))
import Data.Bits

type Index = Int
type Combo = (Index, Index) -- the two source indexes to draw from
type Meal = (Combo, Int) -- which combo, then quantity

-- assume there's 3 inputs
--combos = [(1, 2), (0, 2), (0, 1)]

-- given a set of ingredients, and possible combos, choose a combo and a count
chooseMeals :: [Int] -> Meal
chooseMeals foods = ((smaller, larger), quantity)
    where indexedFoods = foldl' (\list food -> (length list, food) : list) [] foods
          sortedFoods = sortWith snd indexedFoods
          [_, midi, lasty] = sortedFoods
          (smaller, larger) = (min (fst midi) (fst lasty), max (fst midi) (fst lasty))
          quantity = if snd midi > 0 then 1 else 0
    -- subtract the mid from the biggest, i think

sortWith :: Ord b => (a -> b) -> [a] -> [a]
sortWith func list = sortBy (\x y -> compare (func x) (func y)) list

eat :: Meal -> [Int] -> [Int]
eat (combo, quantity) = modifyElement (subtract quantity) (fst combo) . modifyElement (subtract quantity) (snd combo)

modifyElement :: (a -> a) -> Int -> [a] -> [a]
modifyElement _ index [] = error $ "index too big for modifyElement by " ++ show index
modifyElement func 0 (a:as) = (func a) : as
modifyElement func index (a:as) = a : modifyElement func (index - 1) as

allMeals :: [Int] -> [Meal]
allMeals foods =
    if nextQuantity > 0
        then nextMeal : allMeals (eat nextMeal foods)
        else []
    where nextMeal = chooseMeals foods
          (nextCombo, nextQuantity) = nextMeal

main = do
    if length inputs /= 3 then error "wtf" else pure ()
    --print $ foldl (interfood [5,6,9]) [] $ allMeals [5, 6, 9]
    --print $ allMeals [5, 6, 9]
    printMeals (show . fst) inputs $ mealses
    putStrLn "---"
    printMeals (show . id) inputs $ collateMeals mealses
    putStrLn "---"
    mapM_ putStr $ map (++ " ") $ map show $ showCounts $ collateMeals mealses
    where interfood foods [] = []
          interfood foods (meal:meals) = (foods, meal) : interfood (eat meal foods) meals
          mealses = allMeals inputs
          inputs = [5, 6, 9]
          --inputs = [1, 2, 5]

printMeals :: (Meal -> String) -> [Int] -> [Meal] -> IO ()
printMeals _ _ [] = pure ()
printMeals fixer foods (meal:meals) = do
    let foodsLeft = eat meal foods
    putStr $ show foodsLeft
    putStr $ " "
    putStrLn $ show (fixer meal)
    printMeals fixer foodsLeft meals
    
collateMeals :: [Meal] -> [Meal]
collateMeals meals = Map.toList $ foldr (\(combo, quantity) item -> Map.insertWith (+) combo quantity item) (Map.empty) augmentedMeals
    where augmentedMeals = meals ++ [((0, 1), 0), ((0, 2), 0), ((1, 2), 0)]

comboToOrd :: Combo -> Int
comboToOrd (x, y) = (2 ^ x) .|. (2 ^ y)

showCounts :: [Meal] -> [Int]
showCounts = map snd . sortWith (negate . comboToOrd . fst)
