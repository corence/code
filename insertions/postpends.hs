
nums = foldr (\x -> (++ [x])) [] [0..100000]

main = print $ nums !! 99987
