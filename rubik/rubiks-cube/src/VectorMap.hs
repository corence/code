
module VectorMap where

import qualified Data.Vector as V
import Data.Maybe
import Data.Function

type VectorMap k v = V.Vector (k, v)

toList :: VectorMap k v -> [(k, v)]
toList = V.foldr (:) []

-- fromList :: [(k, v)] -> VectorMap k v
-- fromList = V.new

lookup :: Eq k => k -> VectorMap k v -> Maybe v
lookup key
  = listToMaybe
  . V.toList
  . V.map snd
  . V.filter ((== key) . fst)
