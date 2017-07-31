
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

import Data.List

type Plate = [Int] -- c, r, v
type Combos = [(Int, Int)] -- r+v, c+v, c+r

combos :: [(Int, Int)] -> [Int] -- (1, 2), (0, 2), (0, 1)

eatAll :: [Int] -> [(Int, Int)]

chooseMeal :: [(Char, Int)] -> (String, Int)
chooseMeal [] = error "nope"
chooseMeal (food:[]) = error "nape"
chooseMeal foods = 
    -- assume foods is sorted by snd
    where 

eat :: [(Char, Int)] -> [(Char, Int)] -> [(Char, Int)]

main = do
    printCombo (1, 2) result
    printCombo (0, 2) result
    printCombo (0, 1) result
    where result = eatAll [5, 6, 9]

-- subtract from the top 2 numbers but don't let them (combined) drop below the third

5 6 9
5 1 4 -- 5 * rv

1 2 5
1 0 3
