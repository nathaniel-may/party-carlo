-- | A type representing a distribution and concrete confidence intervals
module PartyCarlo.Data.Result where

import Prelude

import Data.Enum (class BoundedEnum, class Enum, Cardinality(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple)
import PartyCarlo.Data.Display (class Display)
import PartyCarlo.Data.SortedArray (SortedArray)


data Interval
    = P90
    | P95
    | P99
    | P999

derive instance eqInterval :: Eq Interval
derive instance ordInterval :: Ord Interval

instance enumInterval :: Enum Interval where
    succ P90 = Just P95
    succ P95 = Just P99
    succ P99 = Just P999
    succ P999 = Nothing

    pred P90 = Nothing
    pred P95 = Just P90
    pred P99 = Just P95
    pred P999 = Just P99 

instance boundedInterval :: Bounded Interval where
    top = P999
    bottom = P90

instance boundedEnumInterval :: BoundedEnum Interval where
    cardinality = Cardinality 4

    toEnum 0 = Just P90
    toEnum 1 = Just P95
    toEnum 2 = Just P99
    toEnum 3 = Just P999
    toEnum _ = Nothing

    fromEnum P90  = 0
    fromEnum P95  = 1
    fromEnum P99  = 2
    fromEnum P999 = 3

instance displayInterval :: Display Interval where
    display P90 = "90%"
    display P95 = "95%"
    display P99 = "99%"
    display P999 = "99.9%"

type Result = 
    { dist :: SortedArray Int
    , p90 :: Tuple Int Int 
    , p95 :: Tuple Int Int 
    , p99 :: Tuple Int Int 
    , p999 :: Tuple Int Int
    }
