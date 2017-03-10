
-- all we want to know is:
-- 1) given a set of towers and a set of enchantments, which is the best value?
-- 2) if we apply it and then repeat the question, then?

import Data.List

data AttributeType = Damage | APS | Special deriving (Show, Eq)
data TowerType = Basic | Cannon deriving (Show, Eq)

data Attribute = Attribute AttributeType Int Float deriving Eq -- type, level, amount
data Enchantment = Enchantment Float TowerType Attribute -- price, towertype, effect
data Tower = Tower TowerType [Attribute] deriving Eq

data State = State Float [Tower] [Enchantment]

sortWith :: Ord b => (a -> b) -> [a] -> [a]
sortWith extract list = sortBy order list
    where order x y = compare (extract x) (extract y)

all_pairs :: [a] -> [b] -> [(a, b)]
all_pairs xs ys = concat $ map (\x -> map (\y -> (x, y)) ys) xs

list_remove :: Eq a => a -> [a] -> [a]
list_remove _ [] = error "didn't find it in the list"
list_remove a (x:xs)
    = if a == x
          then xs
          else x : list_remove a xs

attribute_amount :: Attribute -> Float
attribute_amount (Attribute _ _ amount) = amount

attribute_type :: Attribute -> AttributeType
attribute_type (Attribute a_type _ _) = a_type

tower_get :: AttributeType -> Tower -> Float
tower_get a_type (Tower _ attributes) = value
    where Attribute _ _ value = head $ filter (\attribute -> a_type == attribute_type attribute) attributes

dps :: Tower -> Float
dps tower = (tower_get Damage tower) * (tower_get APS tower)

-- given a tower, find the value increase of an enchantment
value_increase :: Tower -> Enchantment -> Float
value_increase tower enchantment = (dps (try_enchant enchantment tower) - dps tower) / e_price
    where Enchantment e_price _ _ = enchantment

-- given a set of towers and a set of enchantments, find each nonzero increase in value
all_value_increases :: [Tower] -> [Enchantment] -> [(Tower, Enchantment, Float)]
all_value_increases towers enchantments = map (\(enchantment, tower) -> (tower, enchantment, (value_increase tower enchantment))) (all_pairs enchantments towers)

best_value_increase :: [Tower] -> [Enchantment] -> (Tower, Enchantment, Float)
best_value_increase towers enchantments = head $ sortWith (\(_, _, value) -> negate value) (all_value_increases towers enchantments)

try_enchant :: Enchantment -> Tower -> Tower
try_enchant enchantment tower
  | e_tower_type /= t_tower_type = tower
  | otherwise = Tower t_tower_type $ map (try_enchant_attribute e_attribute) t_attributes
  where Enchantment e_price e_tower_type e_attribute = enchantment
        Attribute e_attribute_type e_level e_amount = e_attribute
        Tower t_tower_type t_attributes = tower

try_enchant_attribute :: Attribute -> Attribute -> Attribute
try_enchant_attribute enchant original
  | e_type /= o_type = original
  | e_level /= o_level + 1 = original
  | otherwise = Attribute o_type e_level (o_amount + e_amount)
  where Attribute e_type e_level e_amount = enchant
        Attribute o_type o_level o_amount = original

next :: State -> State
next (State density towers enchantments) = State density new_towers enchantments
    where new_towers = try_enchant best_enchant best_tower : filter ((/=) best_tower) towers
          (best_tower, best_enchant, value_increase) = best_value_increase towers enchantments

initial_state :: State
initial_state = State density towers enchantments
    where density = 1
          towers = [
              Tower Cannon [
                  
              ]
          ]
          enchantments = []

main = return ()
