
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

property_price :: Property -> Rational
property_price (_, 0, _, _, _) = 0
property_price (name, quantity, unit_price, coefficient, unit_dps) = unit_price + property_price (name, (quantity - 1), (coefficient * unit_price), coefficient, unit_dps)

find_milestone_breakeven_time_2 :: Milestone -> Property -> Property -> Rational
find_milestone_breakeven_time_2 milestone prerequisite target = price / dps
    where price = find_milestone_price milestone prerequisite
          dps = find_milestone_dps milestone prerequisite target

find_milestone_price :: Milestone -> Property -> Rational
find_milestone_price milestone prerequisite = property_price (p_name, quantity, p_unit_price, p_coefficient, p_unit_dps)
    where quantity = find_milestone_quantity_needed milestone prerequisite
          (p_name, _, p_unit_price, p_coefficient, p_unit_dps) = prerequisite

find_milestone_dps :: Milestone -> Property -> Property -> Rational
find_milestone_dps milestone prerequisite target = prereq_dps_increase + new_target_dps - old_target_dps
    where 
          new_target_dps = new_target_quantity * target_unit_dps * milestone_multiplier
          old_target_dps = target_quantity * target_unit_dps
          prereq_dps_increase = prereq_unit_dps * prereq_quantity_diff
          prereq_quantity_diff = find_milestone_quantity_needed milestone prerequisite
          (_, _, milestone_quantity, _, milestone_multiplier) = milestone
          (target_name, target_quantity, _, _, target_unit_dps) = target
          (prereq_name, _, _, _, prereq_unit_dps) = prerequisite
          new_target_quantity = if prereq_name == target_name then milestone_quantity else target_quantity

find_milestone_quantity_needed :: Milestone -> Property -> Rational
find_milestone_quantity_needed (_, _, prerequisite_quantity, _, _) (_, property_quantity, _, _, _) = max 0 (prerequisite_quantity - property_quantity)

find_property_dps :: Property -> Rational
find_property_dps (_, quantity, _, _, unit_dps) = quantity * unit_dps
--find_milestone_new_dps () () () = prereq_dps_increase + target_new_dps - target_old_dps
    --where prereq_dps_increase = 

main = do
    let milestone = ("Lemon Scented", "Newspaper", 150, "Lemonade Stand", 100 % 50)
    let lemonade = ("Lemonade Stand", 200, 1, 10 % 11, 2)
    let newspaper = ("Newspaper", 147, 10, 100 % 10, 8)
    putStrLn $ "find_milestone_price: " ++ (show $ fromRational $ find_milestone_price milestone newspaper)
    let old_target_dps = 2 * 200
    let new_target_dps = old_target_dps * 2
    let old_prereq_dps = 8 * 147
    let new_prereq_dps = 8 * 150
    putStrLn $ "target_dps -- old: " ++ show old_target_dps ++ ", new: " ++ show new_target_dps
    putStrLn $ "prereq_dps -- old: " ++ show old_prereq_dps ++ ", new: " ++ show new_prereq_dps
    putStrLn $ "milestone_dps: " ++ (show $ fromRational $ find_milestone_dps milestone newspaper lemonade)
    putStrLn $ "find_milestone_breakeven_time_2: " ++ (show $ fromRational $ find_milestone_breakeven_time_2 milestone newspaper lemonade)
