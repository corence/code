cargo = [21000, 27000, 11800, 13600, 16700, 37000, 45100, 17000, 8500, 19500]

-- for the given set of cargo, find every solution exactly equal to the given target.
-- find_good_cargo_combos target cargo_available cargo_used
find_good_cargo_combos :: Int -> [Int] -> [Int] -> [[Int]]
find_good_cargo_combos _ [] _ = []
find_good_cargo_combos target (cargo:cargos) cargo_used
  | cargo > target = find_good_cargo_combos target cargos cargo_used -- that thing is too big for this target, so try the next
  | cargo == target = [cargo_used]
  | cargo < target = find_good_cargo_combos (target - cargo) cargos (cargo:cargo_used) ++ find_good_cargo_combos target cargos cargo_used -- combine all the results with + without this cargo

main = putStrLn ("coolest cargo combos: " ++ show (find_good_cargo_combos 75000 cargo []))
