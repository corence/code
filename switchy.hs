
append :: Int -> [Int] -> [Int]
append num [] = [num]
append num (x:xs) = x : append num xs

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib num = fib (num - 1) + fib (num - 2)

main = putStrLn $ show $ fib 72


fancy = filter odd (map (+3) (sort [1,4,3,2]))
