import qualified Data.Map as Map
import Data.Map(Map(..))
import Data.List

sortWith :: Ord b => (a -> b) -> [a] -> [a]
sortWith extract list = sortBy order list
    where order x y = compare (extract x) (extract y)

data AttributeType = Damage | APS | Range | Rotation | SplashDamage | SplashRadius
data TowerType = Basic | Cannon

data Attribute = Attribute AttributeType Int Float -- type, level, amount
data Enhancement = Enhancement Int TowerType Attribute -- price, towertype, effect
data Tower = Tower TowerType [Attribute]

data State = State Float [Tower] [Enhancement]

get :: AttributeType -> [Attribute] -> Float
get _ [] = error "can't find attribute"
get type_wanted ((Attribute attribute_type _ value):attributes)
  | type_wanted == attribute_type = value
  | otherwise = get type_wanted attributes

uni_dpv :: [Attribute] -> Float
uni_dpv attributes = get Damage attributes * get APS attributes * get Range attributes

aoe_dpv :: Float -> [Attribute] -> Float
aoe_dpv density attributes = density * get SplashRadius attributes * get SplashDamage attributes * get APS attributes * get Range attributes

dpv :: Float -> [Attribute] -> Float
dpv density attributes = uni_dpv attributes + aoe_dpv density attributes

economicality :: Float -> Int -> [Attribute] -> [Attribute] -> Float
economicality density price old_attributes new_attributes = (dpv density new_attributes - dpv density old_attributes) / price

enhancement_economicality :: Float -> [Tower] -> Enhancement -> (Enhancement, Float)
enhancement_economicality density towers enhancement = enhancement_economicality_increase density enhancement (find_tower enhancement towers)
    where find_tower (Enhancement _ wanted_type _) towers = head $ filter (\(Tower tower_type _) -> tower_type == wanted_type)

enhancement_economicality_increase :: Float -> Tower -> Enhancement -> (Enhancement, Float)    
enhancement_economicality_increase density tower enhancement = economicality density price attributes (apply_enhancement density effect attributes)
    where Tower _ attributes = tower
          Enhancement price _ effect = enhancement

apply_enhancement :: Float -> Attribute -> [Attribute] -> [Attribute]
apply_enhancement _ _ [] = []
apply_enhancement density effect (attribute : attributes)
  | e_type == a_type && e_level == a_level + 1 = Attribute a_type e_level e_amount : attributes
  | otherwise = attribute : apply_enhancement density effect attributes
  where Attribute e_type e_level e_amount = effect
        Attribute a_type a_level a_amount = attribute

-- find the best enhancement and pick it
next_enhancement :: Float -> [Tower] -> [Enhancement] -> (Enhancement, Float)
next_enhancement density towers enhancements = head $ sortWith snd $ map (enhancement_economicality density towers) enhancements

next_state :: Float -> [Tower] -> [Enhancement] -> [Tower]
next_state density towers enhancements = towers_apply_enhancement $ next_enhancement density towers enhancements
