import Control.Monad.Writer

divide2 :: Integer -> Integer -> (Integer, Float)
divide2 dividend divisor = (integer_result, inaccuracy)
    where integer_result = quot dividend divisor
          float_result = (fromIntegral dividend) / (fromIntegral divisor)
          inaccuracy = float_result - (fromIntegral integer_result)
          
divide3 :: Integer -> Integer -> Writer Integer Float
divide3 dividend divisor = writer (divide2 dividend divisor)
-- it's the same as divide2 but it wraps the result in our wrapper type, Writer

dollars_per_year :: Integer
dollars_per_year = 100000

dollars_per_day_with_error :: Writer Integer Float
dollars_per_day_with_error = divide3 dollars_per_year 365

dollars_per_hour_with_error :: Writer Integer Float
dollars_per_hour_with_error = (result2, inaccuracy1 + inaccuracy2)
    where (dollars_per_day, inaccuracy1) = dollars_per_day_with_error
          (result2, inaccuracy2) = divide3 dollars_per_day 24

main = return ()
