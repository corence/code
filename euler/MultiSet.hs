
module MultiSet
( MultiSet(..)
, fromList
, increment
, query
) where

import qualified Data.Map as Map
import Data.Map(Map(..))

type MultiSet a = Map a Int

filter :: Ord a => ((a, Int) -> Bool) -> MultiSet a -> MultiSet a
filter predicate = fromList . Prelude.filter predicate . toList 

q1 :: MultiSet a -> [(a, Int)]
q1 array = (toList array)

fromList :: Ord a => [(a, Int)] -> MultiSet a
fromList = Map.fromList

increment :: Ord a => a -> MultiSet a -> MultiSet a
increment key counters = Map.insertWith (+) key 1 counters

query :: Ord a => a -> MultiSet a -> Int
query key counters = maybe 0 id (Map.lookup key counters)

toList :: MultiSet a -> [(a, Int)]
toList = Map.toList
