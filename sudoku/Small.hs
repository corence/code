
import Data.List

main = putStr . unlines . map disp . solve . return . input =<< getContents

solve s = foldr (\p l -> [mark (p,n) s | s <- l, n <- s p]) s idx

mark (p@(i,j),n) s q@(x,y)
    | p == q = [n]
    | x == i || y == j || e x i && e y j = delete n (s q)
    | otherwise = s q
    where e a b = div (a - 1) 3 == div (b - 1) 3

disp s = unlines [unwords [show $ head $ s (i, j) | j <- [1..9]] | i <- [1..9]]

input s = foldr mark (const [1..9]) $
    [(p,n) | (p,n) <- zip idx $ map read $ lines s >>= words, n > 0]

idx = [(i, j) | i <- [1..9], j <- [1..9]]
