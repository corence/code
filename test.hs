rev [] = []
rev xs = rev2 xs []

rev2 [] ys = ys
rev2 (x:xs) (ys) = rev2 xs (x:ys)

main = do
    putStrLn $ show $ rev [1,3,2,4,5,2]
