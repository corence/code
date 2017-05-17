{-# LANGUAGE DeriveFunctor #-}

module ZipList
( ZipList(..)
) where

class Mappable m where
    mappy :: (a -> b) -> m a -> m b

class Apple a where
    unit :: x -> a x
    apply :: a (x -> y) -> a x -> a y

instance Mappable [] where
    mappy func [] = []
    mappy func (x:xs) = func x : mappy func xs

instance Apple [] where
    unit x = [x]
    apply [] _ = []
    apply (func:funcs) xs = mappy func xs ++ apply funcs xs

data List a = Empty | Cons a (List a)

(<<--->>) :: List a -> List a -> List a
(<<--->>) Empty list = list
--(<<--->>) (Cons value lhs) rhs = (<<--->>) lhs (Cons value rhs) -- this is lazy mode list append -- it reverses the left list. because bah!
(<<--->>) (Cons value lhs) rhs = Cons value $ lhs <<--->> rhs -- this is correct append. not as hard as i thunk

instance Functor List where
    fmap _ Empty = Empty
    fmap func (Cons a as) = Cons (func a) (fmap func as)

instance Applicative List where
    pure value = Cons value Empty
    (<*>) Empty _ = Empty
    (<*>) (Cons func fs) values = fmap func values <<--->> (<*>) fs values

instance Monad List where
    (>>=) Empty _ = Empty
    (>>=) (Cons a as) aToLB = aToLB a <<--->> (as >>= aToLB)

data ZipList a = ZipList [a] deriving Functor
zipListInsert a (ZipList xs) = ZipList (a : xs)

instance Applicative ZipList where
    pure value = ZipList [value]
    -- <*> :: [a -> b] -> [a] -> [b]
    (<*>) (ZipList []) _ = ZipList []
    (<*>) _ (ZipList []) = ZipList []
    (<*>) (ZipList (func:funcs)) (ZipList (value:values)) = zipListInsert (func value) (ZipList funcs <*> ZipList values)
    -- [+ 3, + 4] <*> [5, 21] = [8, 24, 9, 25]
    -- ZipList [+ 3] <*> ZipList 
    
    -- func could be (\5 -> ZipList [True, True, True, True, True]
instance Monad ZipList where
    (>>=) (ZipList []) _ = ZipList []
    (>>=) (ZipList (a:as)) aToZB = ZipList (lhs ++ rhs)
        where (ZipList lhs) = aToZB a
              (ZipList rhs) = ZipList as >>= aToZB

checkMonad :: [Bool]
checkMonad = [3, 4] >>= (flip replicate) True

checkZMonad :: ZipList Bool
checkZMonad = ZipList [3, 4] >>= (\n -> ZipList ((flip replicate) True n))
