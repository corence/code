
type Choice a = [a]

choose :: [a] -> Choice a
choose xs = xs

pair456 :: Int -> Choice (Int, Int)
pair456 x = choose [(x,4), (x,5), (x,6)]

join :: Choice (Choice a) -> Choice a
join choices = concat choices

bind :: Choice Int -> (Int -> Choice (Int, Int)) -> Choice (Int, Int)
bind items function = join (map function items)

unit :: a -> Choice a
unit a = choose [a]

solution1 = map pair456 ([1,2,3])
solution2 = join (map pair456 ([1,2,3]))
solution3 = bind ([1,2,3]) pair456
solution4a x = map (\y -> unit (x,y)) ([4,5,6])
solution4b = map (\x -> map (\y -> unit (x,y)) [4,5,6]) [1,2,3]
solution4  = map (\x -> map (\y -> unit (x,y)) [4,5,6]) [1,2,3]
solution5 = bind
              ([1,2,3])
              (\x ->
                bind
                  ([4,5,6])
                  (\y ->
                    unit (x,y)
                  )
              )

{-
 - function solution4() {
 -     bind(choose([1,2,3]), function(x) {
 -         bind(choose([4,5,6]), function(y) {}}}
 -             
 -}
 

main :: IO ()
main = do
    --let q = solution1
    --let q = solution2
    --let q = solution3
    --mapM_ (\f -> do
                     --let q = f
                     --putStrLn $ (show q)) [solution1,solution2,solution3,solution4a,solution4b,solution4,solution5]
    let q = solution4
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

