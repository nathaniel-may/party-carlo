module PartyCarlo.SortedArray
  ( SortedArray
  , empty
  , filter
  , fromArray
  , head
  , index
  , insert
  , (!!)
  , length
  , tail
  , toArray
  , uncons ) where

import Prelude

import Data.Array as Array
import Data.Foldable (class Foldable)
import Data.Maybe (Maybe(..))


-- | A sorted Array.
-- |
-- | The constructor is private because the undelying array must be sorted first.
newtype SortedArray a = SortedArray (Array a)

instance sortedArraySemigroup ∷ Ord a ⇒ Semigroup (SortedArray a) where
  append xs ys = case uncons ys of
    Nothing -> xs
    Just { head: h, tail: t } -> append (insert h xs) t

derive newtype instance foldableSortedArray ∷ Foldable SortedArray

instance showSortedArray ∷ Show a ⇒ Show (SortedArray a) where
  show = show <<< toArray

empty :: ∀ a. SortedArray a
empty = SortedArray []

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

head :: ∀ a. SortedArray a -> Maybe a
head xs = Array.head $ toArray xs

tail :: ∀ a. SortedArray a -> Maybe (SortedArray a)
tail xs = SortedArray <$> (Array.tail $ toArray xs)

uncons :: ∀ a. SortedArray a -> Maybe { head :: a, tail :: SortedArray a }
uncons xs = (\x y -> { head: x, tail: y }) <$> head xs <*> tail xs

insert :: ∀ a. Ord a => a -> SortedArray a -> SortedArray a
insert x xs@(SortedArray raw) = case uncons xs of 
  Nothing -> SortedArray [x]
  Just { head: h, tail: t } -> 
    if x <= h 
    then SortedArray (Array.cons x raw) 
    else insert h (insert x t)
