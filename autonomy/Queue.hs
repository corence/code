
module Queue
( Queue
, add
, from_list
, is_empty
, pop
, query
, size
, void
) where

-- the first queue is for reading; the second is for insertions
data Queue a = Queue [a] [a]

void :: Queue a
void = Queue [] []

from_list :: [a] -> Queue a
from_list xs = Queue xs []

is_empty :: Queue a -> Bool
is_empty (Queue x y) = null x && null y

size :: Queue a -> Int
size (Queue x y) = length x + length y

add :: a -> Queue a -> Queue a
add a (Queue x y) = Queue x (a : y)

query :: Queue a -> (Queue a, Maybe a)
query queue
  = case pop queue of
    (new_queue, Just result) -> (queue, Just result)
    (old_queue, Nothing) -> (queue, Nothing)

pop :: Queue a -> (Queue a, Maybe a)
pop (Queue [] []) = (Queue [] [], Nothing)
pop (Queue [] y) = pop (Queue (reverse y) [])
pop (Queue (x:xs) y) = (Queue xs y, Just x)
