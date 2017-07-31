
--main = putStrLn $ show $ z
main = fmap show (pure (0 `div` 0)) >>= putStrLn

z :: Int
z = x 0 0

x :: Int -> Int -> Int
x = div
