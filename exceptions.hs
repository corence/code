
--
-- First, let's make our own division function, because we don't like the way division by zero is an instant fail in Haskell

divide1 :: Int -> Int -> Int
divide1 x y = x `div` y

-- test
result1 = divide1 3 4 -- 12
result2 = divide1 3 0 -- program halts

-- test
div_cake_into_groups :: Int -> Int -> Int -> String -- java equivalent of this line: String divvy_cake_into_groups(int num_cakes, int num_groups, int dudes_per_group)
div_cake_into_groups num_cakes num_groups dudes_per_group
    = case (num_cakes `div` num_groups) of
          cakes_per_group -> case (cakes_per_group `div` dudes_per_group) of
              cakes_per_dude -> show num_cakes ++ " cakes between " ++ show num_groups ++ " groups, and each group has " ++ show dudes_per_group ++ " people: " ++ show cakes_per_dude

result3_success = putStrLn (div_cake_into_groups 144 4 3) -- prints 12
result3_fail = putStrLn (div_cake_into_groups 144 4 0) -- program halts






-- Holy shit, dividing by zero kills our program. I'd rather we had exceptions to deal with this, so i can recover when it happens.
data Exception = NullPointerException | ArithmeticException | IOException | ClassCastException

-- Now, any function that wants to throw an exception needs to return this Result type
data Result v = GoodResult v | BadResult Exception -- "v" here is a variable that can take any data type; we'll be using it as a placeholder for "Int". The java equivalent would be "class Result<v> {}"

-- Now we make our new divide function that uses them.
divide2 :: Int -> Int -> Result Int
divide2 _ 0 = BadResult ArithmeticException
divide2 x y = GoodResult (x `div` y)

-- test
result4_success = divide2 3 4 -- this returns (GoodResult 12)
result4_fail = divide2 3 0 -- this returns (BadResult NullPointerException)

--
-- Pretty cool, that's a bit java-like. But it's pretty pointless until we can try/catch.
-- Unlike java's try/catch, this must return a value. So our args are:
--  - contents of the try block; it wants to return a value of type "v"
--  - the catch function, which takes an Exception and produces a) an error string, and b) some fallback / default value
-- Because haskell is lazy, we don't need to wrap the try and catch code in anything special; they'll be executed at the last possible moment
try_catch :: Result v -> (Exception -> v) -> v
try_catch (GoodResult value) _ = value
try_catch (BadResult exception) catch_block = catch_block exception

-- test
divvy_cake_into_groups :: Int -> Int -> Int -> Result String -- java equivalent of this line: String divvy_cake_into_groups(int num_cakes, int num_groups, int dudes_per_group) throws Exception
divvy_cake_into_groups num_cakes num_groups dudes_per_group
    = case (num_cakes `divide2` num_groups) of
        GoodResult cakes_per_group -> case (cakes_per_group `divide2` dudes_per_group) of
            GoodResult cakes_per_dude -> GoodResult (show num_cakes ++ " cakes between " ++ show num_groups ++ " groups, and each group has " ++ show dudes_per_group ++ " people: " ++ show cakes_per_dude)
            BadResult exception -> BadResult exception
        BadResult exception -> BadResult exception

result5_success = putStrLn (try_catch (divvy_cake_into_groups 144 4 3) (\exception -> "Well, that didn't work.")) -- prints 1
result5_fail = putStrLn (try_catch (divvy_cake_into_groups 144 4 0) (\exception -> "Well, that didn't work."))

--
-- holy shit that is a mess. That is totally unsalvageable. Let's salvage it.


-- The basic problem is because we're returning the exception, we need to check the result of every function to see if it returned a value or an exception.
-- Better if we can just implicitly pass all exceptions along to the end of the current function.
-- So, to do this 



main = return ()
