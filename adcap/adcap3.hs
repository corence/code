
-- input: Laws [(Property, amount)]
-- output: Upgrade | (Property, amount)

import Data.Ratio

data Property = Property {
    property_name :: String,
    property_price :: Integer,
    property_profit :: Integer,
    property_coefficient :: Rational,
    property_interval :: Integer,
    property_count :: Integer,
    property_investment :: Integer
}

type PropertyCount = (Property, Integer)

data Upgrade = Upgrade {
    upgrade_name :: String,
    upgrade_price :: Integer,
    upgrade_result :: PropertyModifier
}

data Unlock = Unlock {
    unlock_name :: String,
    unlock_prerequisites :: [PropertyCount],
    unlock_result :: PropertyModifier
}

data ModifierType = ModifierMultiply

data PropertyModifier = PropertyModifier {
    modifier_type :: ModifierType,
    modifier_effect :: Property
}

data Laws = Laws {
    laws_unlocks_available :: [Unlock],
    laws_upgrades_available :: [Upgrade],
    laws_base_properties :: [Property]
}

data PurchaseOrder = PurchaseOrder {
    order_name_of_thing :: String,
    order_price :: Rational,
    order_seconds_until_break_even :: Rational
}

type Investments = ([Property], [Upgrade])

find_best_purchases :: Laws -> Investments -> [PurchaseOrder]
find_best_purchases laws (properties, upgrades) = sorted_merge lockies (sorted_merge units uppies)
    where units = find_best_unit_purchases properties
          lockies = find_best_unlock_purchases (laws_unlocks_available laws) properties
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
          price = calculate_property_price_2 (property_coefficient property) (property_price property) (property_count property) 1
          dps = property_profit property

calculate_property_price :: Rational -> Integer -> Integer -> Integer -> Rational
calculate_property_price _ _ _ 0 = 0
calculate_property_price _ base_price 0 1 = base_price
calculate_property_price coefficient base_price 0 2 = base_price + coefficient * calculate_property_price coefficient base_price 0 1
calculate_property_price coefficient base_price 0 amount_to_buy = base_price + calculate_property_price coefficient (base_price * coefficient) 0 (amount_to_buy - 1)
calculate_property_price coefficient base_price amount_owned amount_to_buy = calculate_property_price coefficient (base_price * coefficient) (amount_owned - 1) amount_to_buy

calculate_property_price_2 :: Rational -> Integer -> Integer -> Rational
calculate_property_price_2 _ _ 0 = 0
calculate_property_price_2 _ base_price 1 = base_price
calculate_property_price_2 coefficient base_price 2 = base_price + coefficient * calculate_property_price_2 coefficient base_price 1
calculate_property_price_2 coefficient base_price amount_to_buy = base_price + calculate_property_price_2 coefficient (base_price * coefficient) (amount_to_buy - 1)

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
          dps = calculate_unlock_dps unlock properties
          
calculate_prerequisite_price :: [Property] -> [PropertyCount] -> Rational          
calculate_prerequisite_price _ [] = 0
--calculate_prerequisite_price (property:properties) (property_count:property_counts) = sorted_merge (calc
--calculate_prerequisite_price (property:properties) property_counts = sorted_merge (calculate_diff_price property property_counts) (calculate_prerequisite_price properties property_counts)

calculate_unlock_dps :: Unlock -> [Property] -> Rational
calculate_unlock_dps _ [] = 0
