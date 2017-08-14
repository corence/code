{-# LANGUAGE BangPatterns #-}

module VectorExample where

import qualified Data.Word as Word
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as M
import qualified Data.Vector.Algorithms.Radix as Radix

-- | Assume 8-bit unsigned integer.
type Value = Word.Word8

-- | What to return as median when there is one.
type MedianValue = Double

-- | This is an obvious implementation, basically the specification.
-- The use of radix sort makes this linear in time, but a copy of
-- the original array is required.
inefficientSpaceMedian :: V.Vector Value -> Maybe MedianValue
inefficientSpaceMedian values
  | V.null values = Nothing
  | odd len = Just (fromIntegral (atSorted midIndex))
  | otherwise = Just (averageValue (atSorted midIndex)
                                   (atSorted (succ midIndex)))
  where
    len = V.length values
    midIndex = pred (succ len `div` 2)

    -- Make a local mutable copy to sort.
    atSorted = V.unsafeIndex (V.modify Radix.sort values)

-- | Average of two values.
averageValue :: Value -> Value -> MedianValue
averageValue a b = (fromIntegral a + fromIntegral b) / 2

average :: V.Vector Value -> Double
average values = (fromIntegral $ V.sum values) / (fromIntegral $ V.length values)

-- | Number of occurrences of an 8-bit unsigned value.
-- We assume no overflow beyond 'Int' range.
type CountOfValue = Int

-- | Create a table of counts for each possible value, since we know
-- the number of values is small and finite.
constantSpaceMedian :: V.Vector Value -> Maybe MedianValue
constantSpaceMedian values
  | V.null values = Nothing
  | odd len = Just (average $ V.map fromIntegral (radixValues 1 midIndex radixes))
  | otherwise = Just (average $ V.map fromIntegral (radixValues 2 midIndex radixes))
  where len = V.length values
        midIndex = ((len + 1) `div` 2) - 1
        radixes = makeCounts values
        --foldr increment (M.replicate 256 0) values

makeCounts :: V.Vector Value -> V.Vector Int
makeCounts values = V.create $ do
    counts <- M.replicate 256 0
    V.forM_ values $
        M.unsafeModify counts succ . fromIntegral
    pure counts

radixValues :: Int -> Int -> V.Vector Int -> V.Vector Int
radixValues count index radixes
  | index <= headValue = V.take count radixes
  | otherwise = radixValues count (index - headValue) (V.tail radixes)
    where headValue = V.head radixes

{-
increment :: Num a => Int -> M.MVector a -> M.MVector a
increment index values = do
    oldCount <- M.read values index
    -- Write back the updated count value
    M.write values index (oldCount + 1)
    -}
