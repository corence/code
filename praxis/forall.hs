
{-# LANGUAGE ScopedTypeVariables #-}

main = print (triangle 3 5)

triangle :: Num a => a -> a -> a
triangle x y = shizzy x y
    where shizzy :: Num a => a -> a -> a
          shizzy = (+)

foob :: forall a b. (b -> b) -> b -> (a -> b) -> Maybe a -> b
foob postProcess onNothin onJust mval =
    postProcess val
    where
        val :: b
        val = maybe onNothin onJust mval
