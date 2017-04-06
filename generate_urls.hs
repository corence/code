
import qualified Data.Map as Map
import Data.Map(Map(..))

type OptionSet = (String, [String]) -- key, can_be_missing, possible_values
type Query = Map String String

scrollable_options = ("scrollable_option", ["", "false", "true"])
vertical_stretch_options = ("vertical_stretch_option", True, ["false", "true"])
types = ("type", False, ["slider_element", "vertical_element"])
items_per_pages = ("items_per_page", True, ["3"])

add_param :: OptionSet -> Query -> [Query]
add_param (name, values) query = map (insert_unless_empty query) values
    where insert_unless_empty query value = if value == ""
                                                then query
                                                else Map.insert name value query
                                                    
makeQueryString :: [(String, String)] -> String
makeQueryString = foldr (\(key, value) result -> "&" ++ key ++ "=" ++ value) ""

main = do
    let query1 = [Map.fromList([])]
    let query2 = query1 >>= add_param ("scrollable_option", ["", "false", "true"])
    let query3 = query2 >>= add_param ("vertical_stretch_option", ["", "false", "true"])
    let query4 = query3 >>= add_param ("type", ["slider_element", "vertical_element"])
    putStrLn $ makeQueryString $ Map.toList =<< query4

--https://docs.vg.learnosity.com/demos/isolation/items_assess.php?&items=vertical_element&scrollable_option=true&vertical_stretch_option=true&items_per_page=2&right=verticaltoc&bottom-right=navigation

