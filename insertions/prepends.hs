
nums = foldr (:) [] [0..100000]

main = print $ nums !! 99987
