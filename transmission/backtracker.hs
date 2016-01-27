
type Choice a = [a]

choose :: [a] -> Choice a
choose xs = xs

pair456 :: Int -> Choice (Int, Int)
pair456 x = choose [(x,4), (x,5), (x,6)]

join :: Choice (Choice a) -> Choice a
join choices = concat choices

main :: IO ()
main = do
    let q = join (map pair456 (choose [1,2,3]))
    putStrLn $ (show q)

{-
solveConstraint = do
    x <- choose [1,2,3]
    y <- choose [4,5,6]
    guard (x*y == 8)
    return (x,y)
    -}

{-
main = do
  z <- solveConstraint
  print z
  
-}

