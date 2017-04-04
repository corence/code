
data Country = Country { country_name :: String, country_average_rainfall :: Float }
data City = City { city_name :: String, city_country :: Maybe Country }
data Dude = Dude { dude_name :: String, dude_city :: Maybe City }

--data City = City String (Maybe Country)
--city_country (City _ country) = country

dude_get_country_name :: Dude -> Maybe String
dude_get_country_name dude
 = case dude_city dude of
   Just city -> case city_country city of
                Just country -> Just (country_name country)
                Nothing -> Nothing
   Nothing -> Nothing

dude_get_country_monadic :: Dude -> Maybe String
dude_get_country_monadic dude = fmap country_name (dude_city dude >>= city_country)
   
main = do
    return ()
