module MonteCarlo where


import Prelude

import Data.Array as Array
import Data.Int (floor, toNumber)
import Data.Maybe (Maybe)
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Random (randomRange)

import Probability (Probability)
import Probability as Prob
import SortedArray (SortedArray, length, (!!))


type Dist = Array Probability

confidenceInterval :: ∀ a. Ord a => Probability -> SortedArray a -> Maybe (Tuple a a)
confidenceInterval p sorted = Tuple <$> low <*> high where
    low = sorted !! floor ((1.0 - Prob.toNumber p) * toNumber len)
    high = sorted !! floor (Prob.toNumber p * toNumber len)
    len = length sorted

sample :: Dist -> Int -> Effect (Array Int)
sample dist count = replicateM count (oneSample dist)

oneSample :: Dist -> Effect Int
oneSample dist = Array.length <<< Array.filter identity <$> traverse check dist

check :: Probability -> Effect Boolean
check p = (_ < Prob.toNumber p) <$> randomRange 0.0 1.0

replicateM :: ∀ m a. Applicative m => Int -> m a -> m (Array a)
replicateM n m = sequence (Array.replicate n m)
