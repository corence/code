
module Util
( sortWith
) where

import Data.List

sortWith :: Ord b => (a -> b) -> [a] -> [a]
sortWith extract list = sortBy order list
    where order x y = compare (extract x) (extract y)

