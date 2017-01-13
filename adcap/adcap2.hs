
-- input: Laws [(Property, amount)]
-- output: Upgrade | (Property, amount)

data Property = Property {
    property_name :: String,
    property_price :: Integer,
    property_profit :: Integer,
    property_coefficient :: Rational,
    property_interval :: Integer
}

type PropertyCount = (Property, Integer)

data Upgrade = Upgrade {
    upgrade_name :: String,
    upgrade_price :: Integer,
    upgrade_result :: PropertyModifier
}

data Unlock = Unlock {
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
    laws_properties :: [Property]
}

data PurchaseOrder = PurchaseOrder {
    order_name_of_thing :: String,
    order_price :: Rational,
    order_seconds_until_break_even :: Rational
}

property_after_modifiers :: [PropertyModifier] -> PropertyCount -> PropertyCount
property_after_modifiers [] property_count = property_count
property_after_modifiers (modifier:modifiers) (Property, Integer)
  | modifier_type modifier == ModifierMultiply = 

modified_property :: Laws -> [Upgrade] -> PropertyCount -> PropertyCount
modified_property laws upgrades property_count = property_after_modifiers modifiers property_count
    where modifiers = map upgrade_result upgrades ++ unlocked_unlocks (laws_unlocks_available law) property_count

find_best_purchases :: Laws -> [Upgrade] -> [PropertyCount] -> [PurchaseOrder]
find_best_purchases laws upgrades property_counts = sorted_merge lockies (sorted_merge units uppies)
    where units = find_best_unit_purchases laws upgrades property_counts
          uppies = find_best_upgrade_purchases laws upgrades property_counts
          lockies = find_best_unlock_purchases laws upgrades property_counts
         
sorted_merge = (++)

find_best_unit_purchases laws upgrades property_counts
