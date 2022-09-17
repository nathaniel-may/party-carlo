module SortedArray (
  SortedArray
  , filter
  , fromArray
  , index
  , (!!)
  , length
  , toArray
) where

import Prelude (class Ord, (<<<))

import Data.Array as Array
import Data.Maybe


-- | A sorted Array.
-- |
-- | The constructor is private because the undelying array must be sorted first.
newtype SortedArray a = SortedArray (Array a)

fromArray :: ∀ a. Ord a => Array a -> SortedArray a
fromArray xs = SortedArray (Array.sort xs)

toArray ::  ∀ a. SortedArray a -> Array a
toArray (SortedArray xs) = xs

length :: ∀ a. SortedArray a -> Int
length = Array.length <<< toArray 

index :: ∀ a. SortedArray a -> Int -> Maybe a
index = Array.index <<< toArray

infixl 8 index as !!

filter :: ∀ a. SortedArray a -> (a -> Boolean) -> SortedArray a
filter xs f = SortedArray (Array.filter f (toArray xs))
