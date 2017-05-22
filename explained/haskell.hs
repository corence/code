import Data.List.Extra

data NameBuilder = NameBuilder {
    names :: [String],
    uppercase :: Bool
}
emptyNameBuilder = NameBuilder { names = [], uppercase = False }

append :: String -> NameBuilder -> NameBuilder
append name nb = nb { names = names nb ++ [name] } -- forget about efficiency in these examples; we're just looking for java/haskell similarity

setUppercase :: Bool -> NameBuilder -> NameBuilder
setUppercase uppercase nb = nb { uppercase = uppercase }

toString :: NameBuilder -> String
toString nb = implode transformedNames
    where transformedNames = if uppercase nb
                                 then map upper (names nb)
                                 else names nb

implode :: [String] -> String
implode [] = ""
implode (string:[]) = string
implode (string:strings) = string ++ " " ++ implode strings

implode1a :: [String] -> String
implode1a strings = init (foldr (\string result -> result ++ string ++ " ") "" strings) -- this looks SUPER weird but "foldr" is actually pretty similar to a for-each loop, if you squint. "init" gets every element in the list except the last one. This isn't meant to be efficient!

implode2 :: [String] -> String
implode2 strings = init (concat (map (++ " ") strings))

implode3 :: [String] -> String
implode3 strings = tail (concat (map (' ' :) strings)) -- this is what i would actually write -- it's reasonably efficient

fullName :: String
fullName = toString result4
    where result4 = setUppercase True result3
          result3 = append "Hedgehog" result2
          result2 = append "The" result1
          result1 = append "Sonic" result0
          result0 = emptyNameBuilder

fullNameChained :: String
fullNameChained = toString (setUppercase True (append "Hedgehog" (append "The" (append "Sonic" emptyNameBuilder))))

main = putStrLn fullName >> putStrLn fullNameChained
