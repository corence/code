
type Choice a = [a]

choose :: [a] -> Choice a
choose xs = xs

solution1 :: Choice (Int, Int)
solution1 = [(1,4), (1,5), (1,6), (2,4), (2,5), (2,6), (3,4), (3,5), (3,6)]
-- solution1: [(1,4),(1,5),(1,6),(2,4),(2,5),(2,6),(3,4),(3,5),(3,6)]

pairValues x array = map (\y -> (x, y)) array

solution2 :: Choice (Int, Int)
solution2 = pairValues 1 [4,5,6]
-- solution2: [(1,4),(1,5),(1,6)]

solution3 :: [Choice (Int, Int)]
solution3 = map (\x -> pairValues x [4,5,6]) [1,2,3]
-- solution3: [[(1,4),(1,5),(1,6)],[(2,4),(2,5),(2,6)],[(3,4),(3,5),(3,6)]]

--join :: Choice (Choice a) -> Choice a
join :: [[a]] -> [a]
join choices = concat choices

solution4 :: Choice (Int, Int)
solution4 = join (map (\x -> pairValues x [4,5,6]) [1,2,3])
-- solution4: [(1,4),(1,5),(1,6),(2,4),(2,5),(2,6),(3,4),(3,5),(3,6)]

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
