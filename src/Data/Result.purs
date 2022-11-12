-- | A type representing a distribution and concrete confidence intervals
module PartyCarlo.Data.Result where

import Data.Maybe (Maybe)
import Data.Tuple (Tuple)
import PartyCarlo.Data.Display (class Display)
import PartyCarlo.Data.SortedArray (SortedArray)


data Interval
    = P90
    | P95
    | P99
    | P999

instance displayInterval :: Display Interval where
    display P90 = "p90"
    display P95 = "p95"
    display P99 = "p99"
    display P999 = "p99.9"

type Result = 
    { dist :: SortedArray Int
    , p90 :: Tuple Int Int 
    , p95 :: Tuple Int Int 
    , p99 :: Tuple Int Int 
    , p999 :: Tuple Int Int 
    , showBars :: Maybe Interval
    }
