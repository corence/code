
-- the goal: to write code in this style
-- solveConstraint = do
--   x <- choose [1,2,3]
--   y <- choose [4,5,6]
--   guard (x*y == 8)
--   return (x,y)

-- step 1: implement the functionality
--      1a: generate a list will all combinations of [1,2,3] and [4,5,6]
--      1b: filter elements out of that list
-- step 2: implement the syntax (as close as we can get it without monads)
-- step 3: implement the syntax with monads

type Choice a = [a]

choose :: [a] -> Choice a
choose xs = xs

solution1 :: Choice (Int, Int)
solution1 = [(1,4), (1,5), (1,6), (2,4), (2,5), (2,6), (3,4), (3,5), (3,6)]

pairValues x array = map (\y -> (x, y)) array

solution2 :: Choice (Int, Int)
solution2 = pairValues 1 [4,5,6]

solution3 :: [Choice (Int, Int)]
solution3 = map (\x -> pairValues x [4,5,6]) [1,2,3]

join :: [[a]] -> [a]
join choices = concat choices

solution4 :: Choice (Int, Int)
solution4 = join (map (\x -> pairValues x [4,5,6]) [1,2,3])

interleave :: [[Int]] -> [Int] -> [[Int]]
interleave homers intruders = concat (map (newHomers homers) intruders)
  where newHomer intruder homer = intruder : homer
        newHomers homers intruder = map (newHomer intruder) homers

solution5 :: Choice [Int]
solution5 = interleave [[]] [4,5,6]

solution6 :: Choice [Int]
solution6 = interleave solution5 [1,2,3]

backtracker :: Choice (Int, Int)
backtracker = filter (\(x, y) -> x + y == 8) solution4

unit :: a -> Choice a
unit a = choose [a]

main = do
  putStrLn $ "solution1: " ++ (show solution1)
  putStrLn $ "solution2: " ++ (show solution2)
  putStrLn $ "solution3: " ++ (show solution3)
  putStrLn $ "solution4: " ++ (show solution4)
  putStrLn $ "backtracker: " ++ (show backtracker)
  putStrLn $ "solution5: " ++ (show solution5)
  putStrLn $ "solution6: " ++ (show solution6)
