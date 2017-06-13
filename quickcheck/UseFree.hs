
import Free

list = [1,2,3]

main = putStrLn $ show $ mogrify list

mogrify :: [a] -> [(a, a)]
mogrify list = do
    a <- list
    b <- list
    pure (a, b)

double :: a -> (a, a)
double thing = (thing, thing)
    
meggify :: Maybe a -> Maybe (a, a)
meggify thingo = thingo >>= pure . double

maggify :: (a -> b) -> (a -> (b, b))
maggify func = (\a -> (func a, func a))

transmogrify :: (Monad m) => m a -> m (a, a)
transmogrify ma = do
    a <- ma
    b <- ma
    pure (a, b)
