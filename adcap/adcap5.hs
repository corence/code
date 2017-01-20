
import GHC.Exts

type Milestone = (String, String, Double, String, Double) -- name, prerequisite_property_name, prerequisite_property_quantity, target_property_name, multiplier
type Property = (String, Double, Double, Double, Double) -- name, quantity, unit_price, coefficient, unit_dps
type Upgrade = (String, Double, String, Double) -- name, price, target_property_name, multiplier
data PurchaseType = PTMilestone | PTUnit | PTUpgrade deriving Show
type PurchaseOrder = (PurchaseType, String, Double, Double) -- purchase_type, purchase_name, price, dps_increase
type PurchasePlan = (PurchaseOrder, Double) -- purchase_order, time_to_breakeven

property_matches_selector :: String -> Property -> Bool
property_matches_selector selector (name, _, _, _, _) = name == selector || selector == "All"

property_price :: Property -> Double
property_price (_, 0, _, _, _) = 0
property_price (name, quantity, unit_price, coefficient, unit_dps) = unit_price + property_price (name, (quantity - 1), (coefficient * unit_price), coefficient, unit_dps)

create_purchase_plan :: Double -> Double -> PurchaseOrder -> PurchasePlan
create_purchase_plan current_funds global_dps purchase_order = (purchase_order, find_breakeven_time purchase_order current_funds global_dps)

find_breakeven_time :: PurchaseOrder -> Double -> Double -> Double
find_breakeven_time purchase_order current_funds global_dps = delay_before_purchase + time_to_recover_price
    where (_, _, price, dps_increase) = purchase_order
          delay_before_purchase = max 0 $ (price - current_funds) / (max 1 global_dps)
          time_to_recover_price = price / (max 1 dps_increase)

-- deprecated
find_upgrade_breakeven_time :: Upgrade -> [Property] -> Double
find_upgrade_breakeven_time upgrade properties = price / (max 1 dps)
    where (_, price, _, _) = upgrade
          dps = find_upgrade_dps upgrade properties

find_upgrade_dps :: Upgrade -> [Property] -> Double
find_upgrade_dps _ [] = 0
find_upgrade_dps upgrade (property:properties) = new_dps - old_dps + find_upgrade_dps upgrade properties
    where (_, _, target_property_name, multiplier) = upgrade
          (name, quantity, _, _, unit_dps) = property
          old_dps = quantity * unit_dps
          new_dps = if property_matches_selector target_property_name property
                        then old_dps * multiplier
                        else old_dps

-- deprecated
find_milestone_breakeven_time :: Milestone -> [Property] -> Double
find_milestone_breakeven_time milestone properties = price / (max 1 dps)
    where price = find_milestone_price milestone properties
          dps = find_milestone_dps milestone properties

find_milestone_price :: Milestone -> [Property] -> Double
find_milestone_price _ [] = 0
find_milestone_price milestone (property:properties) = property_price (p_name, diff_quantity, p_unit_price, p_coefficient, p_unit_dps) + find_milestone_price milestone properties
    where diff_quantity = find_milestone_quantity_needed milestone property
          (p_name, _, p_unit_price, p_coefficient, p_unit_dps) = property

find_milestone_dps :: Milestone -> [Property] -> Double          
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

find_milestone_quantity_needed :: Milestone -> Property -> Double
find_milestone_quantity_needed milestone property = if property_matches_selector prerequisite_name property
                                                        then max 0 (prerequisite_quantity - property_quantity)
                                                        else 0
                                                    where (_, prerequisite_name, prerequisite_quantity, _, _) = milestone
                                                          (property_name, property_quantity, _, _, _) = property

find_property_dps :: Property -> Double
find_property_dps (_, quantity, _, _, unit_dps) = quantity * unit_dps
--find_milestone_new_dps () () () = prereq_dps_increase + target_new_dps - target_old_dps
    --where prereq_dps_increase = 

indent = "    "
pretty_list :: Show a => String -> [a] -> String
pretty_list preindent [] = ""
--pretty_list preindent ((y:ys):xs) = pretty_list (preindent ++ indent) (y:ys) ++ pretty_list preindent xs
pretty_list preindent (x:xs) = preindent ++ show x ++ "\n" ++ pretty_list preindent xs

main = do
    xx1

xx1 = do
    putStrLn $ "1-----1"
    let milestone1 = ("Lemon Scented", "Newspaper", 150, "Lemonade Stand", 2)
    let lemonade = ("Lemonade Stand", 200, 1, 10 / 11, 2)
    let newspaper = ("Newspaper", 147, 10, 10, 8)
    let milestone1 = ("Lemon Scented", "Newspaper", 150, "Lemonade Stand", 2)
    putStrLn $ "find_milestone_price: " ++ (show $ find_milestone_price milestone1 [lemonade, newspaper])
    let old_target_dps = 2 * 200
    let new_target_dps = old_target_dps * 2
    let old_prereq_dps = 8 * 147
    let new_prereq_dps = 8 * 150
    let dps_increase = find_milestone_dps milestone1 [lemonade, newspaper]
    putStrLn $ "target_dps -- old: " ++ show old_target_dps ++ ", new: " ++ show new_target_dps
    putStrLn $ "prereq_dps -- old: " ++ show old_prereq_dps ++ ", new: " ++ show new_prereq_dps
    putStrLn $ "milestone_dps: " ++ (show $ find_milestone_dps milestone1 [newspaper, lemonade])
    putStrLn $ "dps increase: " ++ (show $ dps_increase)
    putStrLn $ "find_milestone_breakeven_time: " ++ (show $ find_milestone_breakeven_time milestone1 [newspaper, lemonade])

    putStrLn $ "2-----2"
    let milestone2 = ("Combustible Lemons", "Lemonade Stand", 300, "Lemonade Stand", 2)
    putStrLn $ "find_milestone_price: " ++ (show $ find_milestone_price milestone2 [lemonade])
    let old_target_dps_2 = 2 * 200
    let new_target_dps_2 = old_target_dps * 2
    let old_prereq_dps_2 = 8 * 147
    let new_prereq_dps_2 = 8 * 150
    putStrLn $ "target_dps -- old: " ++ show old_target_dps_2 ++ ", new: " ++ show new_target_dps_2
    putStrLn $ "prereq_dps -- old: " ++ show old_prereq_dps_2 ++ ", new: " ++ show new_prereq_dps_2
    putStrLn $ "milestone_dps: " ++ (show $ find_milestone_dps milestone2 [lemonade])
    putStrLn $ "find_milestone_breakeven_time: " ++ (show $ find_milestone_breakeven_time milestone2 [lemonade])

    putStrLn $ "3-----3"
    let milestone3 = ("Theoretical Economist", "All", 150, "All", 11)
    putStrLn $ "find_milestone_price: " ++ (show $ find_milestone_price milestone3 [lemonade, newspaper])
    putStrLn $ "milestone_dps: " ++ (show $ find_milestone_dps milestone3 [newspaper, lemonade])
    putStrLn $ "find_milestone_breakeven_time: " ++ (show $ find_milestone_breakeven_time milestone3 [newspaper, lemonade])

    putStrLn $ "and then ----- and then"
    let milestones = [("Combustible Lemons", "Lemonade Stand", 300, "Lemonade Stand", 2), ("Theoretical Economist", "All", 150, "All", 11), ("Lemon Scented", "Newspaper", 150, "Lemonade Stand", 2)]
    let upgrades = [("Imported ice cubes", 4000, "Lemonade stand", 3)]
    let properties = [lemonade, newspaper]
    let upgrade_purchase_orders = map purchase_order_from_upgrade upgrades
            where purchase_order_from_upgrade upgrade = (PTUpgrade, name, price, find_upgrade_dps upgrade properties)
                      where (name, price, _, _) = upgrade
    putStrLn $ "upgrade POs " ++ show upgrade_purchase_orders
    let milestone_purchase_orders = map purchase_order_from_milestone milestones
            where purchase_order_from_milestone milestone = (PTMilestone, name, price, dps)
                      where (name, _, _, _, _) = milestone
                            price = find_milestone_price milestone properties
                            dps = find_milestone_dps milestone properties
    putStrLn $ "milestone POs " ++ show milestone_purchase_orders
    let unit_purchase_orders = map purchase_order_from_property properties
            where purchase_order_from_property property = (PTUnit, name, price, dps)
                      where (name, quantity, _, _, unit_dps) = property
                            price = property_price property
                            dps = unit_dps * quantity
    putStrLn $ "unit POs " ++ show unit_purchase_orders
    let purchase_orders = upgrade_purchase_orders ++ milestone_purchase_orders ++ unit_purchase_orders
    putStrLn $ "all POs " ++ show purchase_orders

    let purchase_plans = sortWith (\(_, ttbe) -> ttbe) $ map (create_purchase_plan 8000 100) purchase_orders
    putStrLn $ "all PPs:\n" ++ pretty_list "" (map show purchase_plans)
