-- | Module for running and reasoning about Monte Carlo experiments.
module PartyCarlo.MonteCarlo where


import Prelude

import Data.Array as Array
import Data.Int (floor, toNumber)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import PartyCarlo.Capability.Pack (class Pack)
import PartyCarlo.Data.Probability (Probability)
import PartyCarlo.Data.Probability as Prob
import PartyCarlo.Data.SortedArray (SortedArray, length, (!!))
import PartyCarlo.Data.SortedArray as SortedArray
import PartyCarlo.Utils (replicateM, rng)
import Random.PseudoRandom (Seed)


type Dist = Array Probability

monteCarloConfidenceInterval :: ∀ m. Pack Seed m => Probability -> Int -> Dist -> m (Maybe (Tuple Int Int))
monteCarloConfidenceInterval p count dist = do
    samples <- sample dist count
    pure $ confidenceInterval p (SortedArray.fromArray samples)

confidenceInterval :: ∀ a. Ord a => Probability -> SortedArray a -> Maybe (Tuple a a)
confidenceInterval p _ | Prob.toNumber p <= 0.5 = Nothing
confidenceInterval p sorted = Tuple <$> low <*> high where
    low = sorted !! floor ((1.0 - Prob.toNumber p) * toNumber len)
    high = sorted !! floor (Prob.toNumber p * toNumber len)
    len = length sorted

sample :: ∀ m. Pack Seed m => Dist -> Int -> m (Array Int)
sample dist count = replicateM count (oneSample dist)

oneSample :: ∀ m. Pack Seed m => Dist -> m Int
oneSample dist = Array.length <<< Array.filter identity <$> traverse check dist

check :: ∀ m. Pack Seed m => Probability -> m Boolean
check p = (_ < Prob.toNumber p) <$> rng
