-- | This module is used for common types used throughout the application.
module PartyCarlo.Types where

import Data.Maybe (Maybe)
import Data.Tuple (Tuple)
import PartyCarlo.SortedArray (SortedArray)


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
