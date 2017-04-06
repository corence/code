class Fluffy f where
  furry :: (a -> b) -> f a -> f b

-- Exercise 1
-- Relative Difficulty: 1
instance Fluffy [] where
  furry a_to_b [] = []
  furry a_to_b (x:xs) = a_to_b x : furry a_to_b xs

-- Exercise 2
-- Relative Difficulty: 1
instance Fluffy Maybe where
  furry a_to_b (Just a) = Just (a_to_b a)
  furry a_to_b Nothing = Nothing

-- Exercise 3
-- Relative Difficulty: 5
instance Fluffy ((->) t) where
  furry a_to_b t_to_a = (\arg -> a_to_b (t_to_a arg))

newtype EitherLeft q p = EitherLeft (Either p q) deriving Show
newtype EitherRight p q = EitherRight (Either p q) deriving Show

-- Exercise 4
-- Relative Difficulty: 5
instance Fluffy (EitherLeft t) where
  furry a_to_b (EitherLeft (Left x)) = EitherLeft (Left (a_to_b x))
  furry a_to_b (EitherLeft (Right x)) = EitherLeft (Right x)

-- Exercise 5
-- Relative Difficulty: 5
instance Fluffy (EitherRight t) where
  furry a_to_b (EitherRight (Left x)) = EitherRight (Left x)
  furry a_to_b (EitherRight (Right x)) = EitherRight (Right (a_to_b x))

class Misty m where
  banana :: (a -> m b) -> m a -> m b
  unicorn :: a -> m a
  -- Exercise 6
  -- Relative Difficulty: 3
  -- (use banana and/or unicorn)
  furry' :: (a -> b) -> m a -> m b
  furry' a_to_b wrapped_a = banana (unicorn . a_to_b) wrapped_a

-- Exercise 7
-- Relative Difficulty: 2
instance Misty [] where
  --banana a_to_wrapped_b wrapped_a = concat (map a_to_wrapped_b wrapped_a)
  banana a_to_list_b [] = []
  banana a_to_list_b (a:as) = (a_to_list_b a) ++ (banana a_to_list_b as)
  unicorn value = [value]

-- Exercise 8
-- Relative Difficulty: 2
instance Misty Maybe where
  banana a_to_maybe_b (Just a) = a_to_maybe_b a
  banana a_to_maybe_b Nothing = Nothing
  unicorn a = Just a

-- Exercise 9
-- Relative Difficulty: 6
instance Misty ((->) t) where
  -- banana :: (a -> (t -> b)) -> (t -> a) -> (t -> b) 
  banana a_to__t_to_b t_to_a = (\t -> (a_to__t_to_b (t_to_a t) t)) -- this is probably totally wrong -- we've applied t to 2 different functions :/
  -- unicorn :: a -> (t -> a)
  unicorn a = (\t -> a) -- this throws away the argument. that doesn't seem good at all

-- Exercise 10
-- Relative Difficulty: 6
instance Misty (EitherLeft t) where
  banana a_to_el_b (EitherLeft (Left a)) = a_to_el_b a
  banana a_to_el_b (EitherLeft (Right t)) = EitherLeft (Right t)
  unicorn a = EitherLeft (Left a)

-- Exercise 11
-- Relative Difficulty: 6
instance Misty (EitherRight t) where
  --banana :: (a -> m b) -> m a -> m b
  banana a_to_er_b (EitherRight (Right a)) = a_to_er_b a
  banana a_to_er_b (EitherRight (Left t)) = EitherRight (Left t)
  unicorn a = EitherRight (Right a)

-- Exercise 12
-- Relative Difficulty: 3
jellybean :: (Misty m) => m (m a) -> m a
jellybean m_m_a = banana id m_m_a
-- jellybean (m (m a)) = m a
-- bind :: (a -> m b) -> m a -> m b
-- fmap :: (a -> b) -> m a -> m b
-- unit :: a -> m a

jellybean1 (Just (Just a)) = Just a
jellybean1 (Just Nothing) = Nothing
jellybean1 Nothing = Nothing

jellybeanA :: Misty m => m (m a) -> m a
--jellybeanA uncertain_thing = furry (\m_a -> furry (\a -> 
--jellybeanA m_m_a = banana (\m_a -> banana (\a -> 
jellybeanA m_m_a = banana id m_m_a

jellybean2 [[]] = []
jellybean2 ([]:lists) = jellybean lists
jellybean2 ((e:es):lists) = e : jellybean (es:lists)

-- [[1,2],[3,4]] -> [1,2,3,4]
--jellybeanB :: [[a]] -> [a]
--jellybeanB l_l_a = banana 

--jb_result :: [a] -> 

jellybean3 :: (t -> t -> a) -> (t -> a)
jellybean3 doublefunc = (\t -> (doublefunc t) t)

jellybean4 (EitherRight (Right (EitherRight (Right a)))) = EitherRight (Right a)
jellybean4 (EitherRight (Right (EitherRight (Left a)))) = EitherRight (Left a)
jellybean4 (EitherRight (Left t)) = EitherRight (Left t)

--jellybean12 :: Maybe (Maybe a) -> Maybe a
--jellybean12 param = 

testcase1 :: (Int -> Int)
testcase1 = jellybean3 (+)

-- Exercise 13
-- Relative Difficulty: 6
apple :: (Misty m) => m a -> m (a -> b) -> m b
apple wrapped_a wrapped_func = jellybean $ furry' (\func -> furry' (\a -> func a) wrapped_a) wrapped_func

-- Exercise 14
-- Relative Difficulty: 6
--moppy :: (Misty m) => [a] -> (a -> m b) -> m [b]
--  furry :: (p -> q) -> m p -> m q
--  banana :: (p -> m q) -> m p -> m q
--  unicorn :: p -> m p
--  jellybean :: (Misty m) => m (m p) -> m p
--  apple :: (Misty m) => m p -> m (p -> q) -> m q
--moppy list_of_a a_to_mb = error "todo"

--  furry :: ([b] -> [b]) -> m [b] -> m [b]
--  furry :: (b -> [b]) -> m b -> m [b]
--  furry :: (b -> [b]) -> m b -> m [b]
--  furry :: ([b] -> [b]) -> m [b] -> m [b]
--  furry :: (p -> [b]) -> m p -> m [b]
--  banana :: (p -> m [b]) -> m p -> m [b]
--  jellybean :: m (m [b]) -> m [b]
--  apple :: m p -> m (p -> [b]) -> m [b]
moppy :: (Misty m) => [a] -> (a -> m b) -> m [b]
moppy [] a_to_mb = unicorn []
--moppyA (a:as) a_to_mb = furry' (\new_b -> furry' (new_b :) others) (a_to_mb a)
moppy (a:as) a_to_mb = furry2 (:) (a_to_mb a) others_mlb
    where others_mlb = moppy as a_to_mb

do_thingo :: (Misty m) => m [b] -> b -> m [b]
do_thingo others_mlb unwrapped_converted_a = furry' (unwrapped_converted_a :) others_mlb

thingoA :: b -> [b] -> [b]
thingoA = (:)

moppy1 :: [a] -> (a -> Maybe b) -> Maybe [b]
moppy1 [] _ = Just []
moppy1 (a:as) a_to_mb = case a_to_mb a of
                        Just b -> case moppy1 as a_to_mb of
                                  Just bs -> Just (b : bs)
                                  Nothing -> Nothing
                        Nothing -> Nothing

furry2 :: (Misty m) => (a -> b -> c) -> m a -> m b -> m c
furry2 func m_a m_b = apple m_b (furry' func m_a) 

moppy2 :: [a] -> (a -> (p -> b)) -> (p -> [b])
moppy2 as a_to_func = (\p -> furry (\func -> func p) (furry a_to_func as))

--moppy3 :: Misty m => [a] -> (a -> m b) -> (m [b])
--moppy3 as a_to_func = (\p -> furry (\func -> func p) (furry a_to_func as))

moppy5 :: [a] -> (a -> (EitherRight k b)) -> (EitherRight k [b])
-- convert it to a list of EitherRights
-- if any of them are Left, return one of the Lefts (i dont care which one)
-- otherwise, return a list saying they're all good
-- so we go from [a] -> [EitherRight k b] -> EitherRight k [b]
moppy5 [] _ = EitherRight (Right [])
moppy5 (a:as) a_to_either = case a_to_either a of
                            EitherRight (Left k) -> EitherRight (Left k)
                            EitherRight (Right b) -> case moppy5 as a_to_either of
                                       EitherRight (Left kk) -> EitherRight (Left kk)
                                       EitherRight (Right bs) -> EitherRight (Right (b : bs))

--  / furry :: (a -> b) -> [a] -> [b]
--  furry :: (a -> b) -> [EitherRight k a] -> [EitherRight k b]
--  banana :: (a -> EitherRight k b) -> EitherRight k a -> EitherRight k b
--  unicorn :: a -> EitherRight k a
--  jellybean :: [[a]] -> [a]
--  apple :: [a] -> [a -> b] -> [b]

--  furry :: (a -> b) -> EitherRight k a -> EitherRight k b
--  banana :: (a -> m b) -> m a -> m b
--  unicorn :: a -> m a
--  jellybean :: (Misty m) => m (m a) -> m a
--  apple :: (Misty m) => m a -> m (a -> b) -> m b

--  furry :: (EitherRight k b -> q) -> m p -> m q
--  banana :: (p -> m q) -> m p -> m q
--  unicorn :: p -> m p
--  jellybean :: (Misty m) => m (m p) -> m p
--  apple :: (Misty m) => m p -> m (p -> q) -> m q

--  furry :: (p -> q) -> m p -> m q
--  banana :: (p -> m q) -> m p -> m q
--  unicorn :: p -> m p
--  jellybean :: (Misty m) => m (m p) -> m p
--  apple :: (Misty m) => m p -> m (p -> q) -> m q

-- if we assume it must return a (Maybe [b]):
--  moppy :: [a] -> (a -> Maybe b) -> (Maybe [b])
--  unicorn :: b -> Maybe [b]
--  jellybean :: Maybe (Maybe [b]) -> Maybe [b]
--  apple :: Maybe p -> Maybe (p -> [b]) -> Maybe [b]
--nope because if you need to call unit to get the (Maybe p), then you really didn't need this function at all:
--  banana :: (p -> Maybe [b]) -> Maybe p -> Maybe [b]
--  furry :: (p -> [b]) -> Maybe p -> Maybe [b]

-- if we assume it must return a (Maybe [b]) -- part 2:
--  moppy :: [a] -> (a -> Maybe b) -> (Maybe [b])
--  unicorn :: b -> Maybe [b]
--  jellybean :: Maybe (Maybe [b]) -> Maybe [b]
--  apple :: Maybe p -> Maybe (p -> [b]) -> Maybe [b]
--  banana :: ([b] -> Maybe [b]) -> Maybe [b] -> Maybe [b]
--  furry :: (p -> [b]) -> Maybe p -> Maybe [b]

-- if we assume it must return a (m [b]):
--  moppy :: (Misty m) => [a] -> (a -> m b) -> (m [b])
--  furry :: (p -> [b]) -> m p -> m [b]
--  banana :: (p -> m [b]) -> m p -> m [b]
--  unicorn :: p -> m [b]
--  jellybean :: (Misty m) => m (m [b]) -> m [b]
--  apple :: (Misty m) => m p -> m (p -> [b]) -> m [b]

-- if we assume it must return a (m (n b)):
--  moppy :: (n a) -> (a -> m b) -> (m (n b))
--  furry :: (p -> (n b)) -> m p -> m (n b)
--  banana :: (p -> m (n b)) -> m p -> m (n b)
--  unicorn :: p -> m (n b)
--  jellybean :: m (m (n b)) -> m (n b)
--  apple :: m p -> m (p -> (n b)) -> m (n b)
          
moppy7 :: [a] -> (a -> [b]) -> [[b]]
moppy7 as a_to_bs = furry a_to_bs as

--moppy8 :: Misty m => [a] -> (a -> m b) -> (m [b])
--  apple :: Maybe p -> Maybe (p -> [b]) -> Maybe [b]
--moppy8 as a_to_mb = furry a_to_mb as

moppy9 :: (Misty m) => [a] -> (a -> m b) -> m [b]
moppy9 list_of_a a_to_mb = sausage (furry a_to_mb list_of_a)

--  apple :: Maybe p -> Maybe (p -> [b]) -> Maybe [b]
--mini :: (a, a) -> (a -> Maybe b) -> (Maybe (b, b))

-- Exercise 15
-- Relative Difficulty: 6
-- (bonus: use moppy)
sausage :: (Misty m) => [m a] -> m [a]
--  moppy :: Misty m => [m a] -> (m a -> m a) -> m [a]
--  moppy :: Misty m => [a] -> (a -> m b) -> m [b]
sausage wrapped_things = moppy9 wrapped_things id

sausage1 :: [Maybe a] -> Maybe [a]
sausage1 [] = Nothing
sausage1 (Nothing : as) = sausage1 as
sausage1 (Just a : as) = case sausage1 as of
                             Just list -> Just (a : list)
                             Nothing -> Just [a]

sausage2 :: [t -> a] -> (t -> [a])
sausage2 funcs = (\t -> furry (\func -> func t) funcs)

sausage3 :: [EitherRight t a] -> EitherRight t [a]
sausage3 [] = EitherRight (Right [])
sausage3 ((EitherRight (Left result)) : _) = EitherRight (Left result)
sausage3 ((EitherRight (Right value)) : es) = case (sausage3 es) of
                                                  EitherRight (Left result) -> EitherRight (Left result)
                                                  EitherRight (Right values) -> EitherRight (Right (value : values))
                                                  
sausage4 :: (Misty m) => [m a] -> m [a]
sausage4 [] = unicorn []
sausage4 (wrapped_head : wrapped_things) = banana (inject4 wrapped_head) new_things
    where new_things = sausage4 wrapped_things
          
inject4 :: (Misty m) => m a -> [a] -> m [a]
inject4 wrapped_head new_thing = furry' (\x -> x : new_thing) wrapped_head

sausage5 :: [[a]] -> [[a]]
sausage5 [] = return []
sausage5 (wrapped_head : wrapped_things) = new_things >>= inject5 wrapped_head
    where new_things = sausage5 wrapped_things
          
--inject :: (Misty m) => m a -> m [a]
--inject wrapped_head new_thing = wrapped_head >>= (: new_thing)

--banana :: (a -> m b) -> m a -> m b
--banana2 :: (Misty m) => (a -> (b -> c)) -> m a -> (m b -> m c)
--apple :: (Misty m) => m a -> m (a -> b) -> m b
--furry' :: (Misty m) => (a -> b) -> m a -> m b
inject5 :: [a] -> [a] -> [[a]]
inject5 wrapped_head new_thing = furry (\x -> x : new_thing) wrapped_head

--sausage4 :: (Misty m) => [m a] -> m [a]
--sausage4 [] = error "wtf"
--sausage4 (m_a:m_as) = m_a >>= unicorn (\a -> a : sausage4 m_as)

--sss :: m a -> m [a] -> m [a]
--sss m_a m_as = furry (:) m_a m_as

--sequence [[1,2,3],[4,5,6],[7,8,9]]
--[[1,4,7],[1,4,8],[1,4,9],[1,5,7],[1,5,8],[1,5,9],[1,6,7],[1,6,8],[1,6,9],[2,4,7],[2,4,8],[2,4,9],[2,5,7],[2,5,8],[2,5,9],[2,6,7],[2,6,8],[2,6,9],[3,4,7],[3,4,8],[3,4,9],[3,5,7],[3,5,8],[3,5,9],[3,6,7],[3,6,8],[3,6,9]]

--sequence [[1,2],[4,5],[7,8]]
--[[1,4,7],[1,4,8],[1,5,7],[1,5,8],[2,4,7],[2,4,8],[2,5,7],[2,5,8]]

--sequence [[1,2,3,4,5,6],[4,5]]
--[[1,4],[1,5],[2,4],[2,5],[3,4],[3,5],[4,4],[4,5],[5,4],[5,5],[6,4],[6,5]]
--cartesian_product :: [[a]] -> [[a]]
--cartesian_product lists = foreach lists as list -> foreach element -> use it
--[[1,2],[4,5]]
--[[1,4],[4,5]]
-- foreach (list, other_lists)
--   foreach
--cartesian_product :: [[a]] -> [[a]]
-- prepend everything in list to everything in lists
-- lists is [[a]] but it will become [[[a]]]
--cartesian_product (list : lists) = list >>= (:)
--cartesian_product [] = []
--cartesian_product (list : lists) = list >>= (\x -> fmap (:) x (cartesian_product lists))

-- Exercise 16
-- Relative Difficulty: 6
-- (bonus: use apple + furry')
banana2 :: (Misty m) => (a -> b -> c) -> m a -> m b -> m c
banana2 func w_a w_b = apple w_b (furry' func w_a)

-- Exercise 17
-- Relative Difficulty: 6
-- (bonus: use apple + banana2)
banana3 :: (Misty m) => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
--banana :: (a -> m b) -> m a -> m b
--banana2 :: (Misty m) => (a -> (b -> c)) -> m a -> (m b -> m c)
--apple :: (Misty m) => m a -> m (a -> b) -> m b
--furry' :: (Misty m) => (a -> b) -> m a -> m b
banana3 func w_a w_b w_c = apple w_c (banana2 func w_a w_b)

-- Exercise 18
-- Relative Difficulty: 6
-- (bonus: use apple + banana3)
banana4 :: (Misty m) => (a -> b -> c -> d -> e) -> m a -> m b -> m c -> m d -> m e
banana4 func w_a w_b w_c w_d = apple w_d (banana3 func w_a w_b w_c)

newtype State s a = State {
  state :: (s -> (s, a))
}

-- Exercise 19
-- Relative Difficulty: 9
instance Fluffy (State s) where
  --furry :: (a -> b) -> f a -> f b
  --furry :: (a -> b) -> State (s -> (s, a)) -> State (s -> (s, b))
  furry func transform = State { state = (\initial_state -> let (second_state, value_a) = (state transform) initial_state
                                                                value_b = func value_a
                                                                in (second_state, value_b)) }

-- Exercise 20
-- Relative Difficulty: 10
instance Misty (State s) where
  --banana :: (a -> m b) -> m a -> m b
  --banana :: (a -> State s b) -> State s a -> State s b
  banana func transform = State { state = (\initial_state -> let (second_state, value_a) = (state transform) initial_state
                                                                 second_transform = func value_a
                                                                 in (state second_transform) second_state) }
  unicorn x = State { state = (\s -> (s, x)) }

main = do return ()
