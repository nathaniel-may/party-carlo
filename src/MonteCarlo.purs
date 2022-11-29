-- | Module for running and reasoning about Monte Carlo experiments.
-- TODO refactor to use prng with seeds for deterministic testing
module PartyCarlo.MonteCarlo where


import Prelude

import Control.Parallel (parSequence)
import Data.Array as Array
import Data.Either (either)
import Data.Foldable (foldM)
import Data.Int (floor, toNumber)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Random (random)
import PartyCarlo.Data.Probability (Probability, mkProbability)
import PartyCarlo.Data.Probability as Prob
import PartyCarlo.Data.SortedArray (SortedArray, length, (!!))
import PartyCarlo.Data.SortedArray as SortedArray
import PartyCarlo.Utils (if', replicateM)


type Dist = Array Probability

monteCarloConfidenceInterval :: Probability -> Int -> Dist -> Effect (Maybe (Tuple Int Int))
monteCarloConfidenceInterval p count dist = do
    samples <- sample count dist
    pure $ confidenceInterval p (SortedArray.fromArray samples)

confidenceInterval :: ∀ a. Ord a => Probability -> SortedArray a -> Maybe (Tuple a a)
confidenceInterval p _ | Prob.toNumber p <= 0.5 = Nothing
confidenceInterval p sorted = Tuple <$> low <*> high where
    low = sorted !! floor ((1.0 - Prob.toNumber p) * toNumber len)
    high = sorted !! floor (Prob.toNumber p * toNumber len)
    len = length sorted

parSample :: Int -> Dist -> Aff (Array Int)
parSample count dist = parSequence $ Array.replicate count (liftEffect $ oneSample dist)

sample :: Int -> Dist -> Effect (Array Int)
sample count dist = replicateM count (oneSample dist)

oneSample :: Dist -> Effect Int
oneSample = foldM (\count d -> (\p -> (if' (p < d) (count + 1) count)) <$> rand) 0

rand :: Effect Probability
rand = either (\_ -> rand) pure <<< mkProbability =<< random
