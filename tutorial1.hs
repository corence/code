
import qualified Data.Map as Map
import Data.Map(Map(..))

-- for each "city", we are storing the name of its country if it exists inside one
cities = Map.fromList [("Sydney", (Just "Australia")), ("Darwin", (Just "Australia")), ("The ISS", Nothing)] :: Map String (Maybe String)

-- we have new business requirements -- for some records we're going to want to add the planet name to the country name
-- if it doesn't have a country name, we'll ignore this and do nothing
set_planet_name :: String -> String -> Map String (Maybe String) -> Map String (Maybe String)
set_planet_name planet_name city_name cities
    = case Map.lookup city_name cities of
      Just country_name -> Map.insert city_name (Just (country_name ++ ", " ++ planet_name)) cities
      Nothing -> cities
      
main = do
    cities2 <- set_planet_name "Earth" "Sydney" cities
    cities3 <- set_planet_name "Space" "The ISS" cities
    return ()
