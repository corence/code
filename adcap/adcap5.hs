
import Data.Ratio

-- to evaluate if an unlock is worth buying
-- input:
--  - price
--  - dps
-- output:
--  - time to break even

-- to find the price of an unlock
-- input:
--  - prerequisite_property_count
--  - property_current_count
--  - property_unit_price
--  - property_coefficient
-- output:
--  - price

-- to find the dps of an unlock
-- input:
--  - dps_per_unit
--  - property_current_count
--  - prerequisite_property_count
--  - property_multiplier
-- output:
--  - dps

purchase_compute_breakeven price dps = price % dps

property_price coefficient unit_price 0 = 0
property_price coefficient unit_price quantity = unit_price + property_price coefficient (coefficient * unit_price) (quantity - 1)

unlock_price prerequisite_property_quantity property_current_quantity property_coefficient property_unit_price
  | (prerequisite_property_quantity <= property_current_quantity) = 0
  | otherwise = property_price property_coefficient property_unit_price (prerequisite_property_quantity - property_current_quantity)

property_dps unit_dps multiplier quantity = unit_dps * multiplier * quantity

unlock_breakeven prerequisite_property_quantity property_current_quantity property_coefficient property_unit_price

-- yeah bitches









-- input: Laws [(Property, amount)]
-- output: Upgrade | (Property, amount)


data Property = Property {
    property_name :: String,
    property_coefficient :: Rational,
    property_unit_price :: Integer,
    property_current_price :: Integer,
    property_unit_profit :: Integer,
    property_current_profit :: Integer,
    property_current_interval :: Integer,
    property_count :: Integer,
    property_cash_invested :: Integer
}

data Upgrade = Upgrade {
    upgrade_name :: String,
    upgrade_price :: Integer,
    upgrade_result :: PropertyModifier
}

data Unlock = Unlock {
    unlock_name :: String,
    unlock_prerequisites :: [(String, Integer)],
    unlock_result :: PropertyModifier
}

data ModifierType = ModifierMultiply

data PropertyModifier = PropertyModifier {
    modifier_type :: ModifierType,
    modifier_effect :: Property
}

type Laws = ([Unlock], [Upgrade], [Property])

data PurchaseOrder = PurchaseOrder {
    order_name_of_thing :: String,
    order_price :: Rational,
    order_seconds_until_break_even :: Rational
}

type Investments = ([Property], [Upgrade])

find_best_purchases :: Laws -> Investments -> [PurchaseOrder]
find_best_purchases (unlocks_available, upgrades_available, _base_properties) (properties, upgrades) = sorted_merge lockies (sorted_merge units uppies)
    where units = find_best_unit_purchases properties
          lockies = find_best_unlock_purchases unlocks_available properties
          uppies = find_best_upgrade_purchases laws upgrades property_counts
         
sorted_merge = (++)

find_best_unit_purchases :: [Property] -> [PurchaseOrder]
find_best_unit_purchases [] = []
find_best_unit_purchases (property:properties) = sorted_merge [purchase_order] (find_best_unit_purchases properties)
    where purchase_order = PurchaseOrder {
              order_name_of_thing = property_name property,
              order_price = price,
              order_seconds_until_break_even = price % dps
          }
          price = property_current_price property
          dps = property_unit_profit property % property_current_interval property

calculate_property_price :: Property -> Integer -> Rational
calculate_property_price property amount_to_buy = calculate_coefficient_price coefficient base_price amount_to_buy
    where coefficient = property_coefficient property
          base_price = property_current_price property

calculate_coefficient_price :: Rational -> Integer -> Integer -> Rational
calculate_coefficient_price _ _ 0 = 0
calculate_coefficient_price coefficient base_price amount_to_buy = base_price + calculate_coefficient_price coefficient (base_price * coefficient) (amount_to_buy - 1)

-- for each unlock, find the price
find_best_unlock_purchases :: [Unlock] -> [Property] -> [PurchaseOrder]
find_best_unlock_purchases [] _ = []
find_best_unlock_purchases (unlock:available_unlocks) properties = sorted_merge [purchase_order] (find_best_unlock_purchases available_unlocks properties)
    where purchase_order = PurchaseOrder {
              order_name_of_thing = unlock_name unlock,
              order_price = price,
              order_seconds_until_break_even = price % dps
          }
          price = calculate_unlock_price unlock properties
          dps = foldr (calculate_unlock_dps unlock) 0  properties

calculate_unlock_price :: Unlock -> [Property] -> Rational
calculate_unlock_price unlock properties = foldr (calculate_prerequisite_price properties) 0 prerequisites
    where prerequisites = unlock_prerequisites unlock
          
calculate_prerequisite_price :: [Property] -> (String, Integer) -> Rational
calculate_prerequisite_price (property:properties) (pname, pcount) =
    if (pname == (property_name property)) && (property_count property < pcount)
        then calculate_property_price property (pcount - (property_count property))
        else 0
        
calculate_unlock_dps :: Unlock -> Property -> Rational
calculate_unlock_dps unlock property =
    if pname == (property_name property)
        then property_current_profit adjusted_property % property_current_interval adjusted_property
        else 0
