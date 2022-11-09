module PartyCarlo.Types where

import Prelude

import Data.Maybe (Maybe)
import Data.Tuple (Tuple)
import SortedArray (SortedArray)


data Interval
  = P90
  | P95
  | P99
  | P999

type Result = 
  { dist :: SortedArray Int
  , p90 :: Tuple Int Int 
  , p95 :: Tuple Int Int 
  , p99 :: Tuple Int Int 
  , p999 :: Tuple Int Int 
  , showBars :: Maybe Interval
  }
