
import Data.Ratio

-- to evaluate if a milestone is worth buying
-- input:
--  - price
--  - dps
-- output:
--  - time to break even

-- to find the price of a milestone
-- input:
--  - prerequisite_property_count
--  - property_current_count
--  - property_unit_price
--  - property_coefficient
-- output:
--  - price

-- to find the dps of a milestone
-- input:
--  - dps_per_unit
--  - property_current_count
--  - prerequisite_property_count
--  - property_multiplier
-- output:
--  - dps

type Milestone = (String, String, Rational, String, Rational) -- name, prerequisite_property_name, prerequisite_property_quantity, target_property_name, multiplier
type Property = (String, Rational, Rational, Rational, Rational) -- name, quantity, unit_price, coefficient, unit_dps

property_matches_selector :: String -> Property -> Bool
property_matches_selector selector (name, _, _, _, _) = name == selector || selector == "All"

property_price :: Property -> Rational
property_price (_, 0, _, _, _) = 0
property_price (name, quantity, unit_price, coefficient, unit_dps) = unit_price + property_price (name, (quantity - 1), (coefficient * unit_price), coefficient, unit_dps)

find_milestone_breakeven_time :: Milestone -> [Property] -> Rational
find_milestone_breakeven_time milestone properties = price / dps
    where price = find_milestone_price milestone properties
          dps = find_milestone_dps milestone properties

find_milestone_price :: Milestone -> [Property] -> Rational
find_milestone_price _ [] = 0
find_milestone_price milestone (property:properties) = property_price (p_name, diff_quantity, p_unit_price, p_coefficient, p_unit_dps) + find_milestone_price milestone properties
    where diff_quantity = find_milestone_quantity_needed milestone property
          (p_name, _, p_unit_price, p_coefficient, p_unit_dps) = property

find_milestone_dps :: Milestone -> [Property] -> Rational          
find_milestone_dps _ [] = 0
find_milestone_dps milestone (property:properties) = (new_quantity * multiplied_dps) - (quantity * unit_dps) + find_milestone_dps milestone properties
    where (_, prerequisite_property_name, prerequisite_property_quantity, target_property_name, multiplier) = milestone
          (property_name, quantity, unit_price, coefficient, unit_dps) = property
          new_quantity = if property_matches_selector prerequisite_property_name property && quantity < prerequisite_property_quantity
                             then prerequisite_property_quantity
                             else quantity
          multiplied_dps = if property_matches_selector target_property_name property
                               then unit_dps * multiplier
                               else unit_dps

find_milestone_quantity_needed :: Milestone -> Property -> Rational
find_milestone_quantity_needed milestone property = if property_matches_selector prerequisite_name property
                                                        then max 0 (prerequisite_quantity - property_quantity)
                                                        else 0
                                                    where (_, prerequisite_name, prerequisite_quantity, _, _) = milestone
                                                          (property_name, property_quantity, _, _, _) = property

find_property_dps :: Property -> Rational
find_property_dps (_, quantity, _, _, unit_dps) = quantity * unit_dps
--find_milestone_new_dps () () () = prereq_dps_increase + target_new_dps - target_old_dps
    --where prereq_dps_increase = 

main = do
    let milestone = ("Lemon Scented", "Newspaper", 150, "Lemonade Stand", 100 % 50)
    let lemonade = ("Lemonade Stand", 200, 1, 10 % 11, 2)
    let newspaper = ("Newspaper", 147, 10, 100 % 10, 8)
    putStrLn $ "find_milestone_price: " ++ (show $ fromRational $ find_milestone_price milestone [lemonade, newspaper])
    let old_target_dps = 2 * 200
    let new_target_dps = old_target_dps * 2
    let old_prereq_dps = 8 * 147
    let new_prereq_dps = 8 * 150
    let dps_increase = find_milestone_dps milestone [lemonade, newspaper]
    putStrLn $ "target_dps -- old: " ++ show old_target_dps ++ ", new: " ++ show new_target_dps
    putStrLn $ "prereq_dps -- old: " ++ show old_prereq_dps ++ ", new: " ++ show new_prereq_dps
    putStrLn $ "milestone_dps: " ++ (show $ fromRational $ find_milestone_dps milestone [newspaper, lemonade])
    putStrLn $ "dps increase: " ++ (show $ fromRational $ dps_increase)
    putStrLn $ "find_milestone_breakeven_time: " ++ (show $ fromRational $ find_milestone_breakeven_time milestone [newspaper, lemonade])

    putStrLn $ "2-----2"
    let milestone2 = ("Combustible Lemons", "Lemonade Stand", 300, "Lemonade Stand", 2)
    putStrLn $ "find_milestone_price: " ++ (show $ fromRational $ find_milestone_price milestone2 [lemonade])
    let old_target_dps_2 = 2 * 200
    let new_target_dps_2 = old_target_dps * 2
    let old_prereq_dps_2 = 8 * 147
    let new_prereq_dps_2 = 8 * 150
    putStrLn $ "target_dps -- old: " ++ show old_target_dps_2 ++ ", new: " ++ show new_target_dps_2
    putStrLn $ "prereq_dps -- old: " ++ show old_prereq_dps_2 ++ ", new: " ++ show new_prereq_dps_2
    putStrLn $ "milestone_dps: " ++ (show $ fromRational $ find_milestone_dps milestone2 [lemonade])
    putStrLn $ "find_milestone_breakeven_time: " ++ (show $ fromRational $ find_milestone_breakeven_time milestone2 [lemonade])
    
    putStrLn $ "3-----3"
    let milestone3 = ("Theoretical Economist", "All", 150, "All", 11)
    putStrLn $ "find_milestone_price: " ++ (show $ fromRational $ find_milestone_price milestone3 [lemonade, newspaper])
    putStrLn $ "milestone_dps: " ++ (show $ fromRational $ find_milestone_dps milestone3 [newspaper, lemonade])
    putStrLn $ "find_milestone_breakeven_time: " ++ (show $ fromRational $ find_milestone_breakeven_time milestone3 [newspaper, lemonade])
