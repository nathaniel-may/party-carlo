module PartyCarlo.Data.Probability (
    Probability
    , p90
    , p95
    , p99
    , p999
    , probability
    , toNumber
) where

import Prelude

import Data.Either (Either(..))
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck.Gen (choose)


data Probability = Probability Number

probability :: Number -> Either Number Probability
probability p = if p <= 1.0 && p >= 0.0 then Right (Probability p) else Left p

toNumber :: Probability -> Number
toNumber (Probability x) = x

-- commonly referenced probabilities in source
p90 :: Probability
p90 = Probability 0.9

p95 :: Probability
p95 = Probability 0.95

p99 :: Probability
p99 = Probability 0.99

p999 :: Probability
p999 = Probability 0.999

instance arbProbability :: Arbitrary Probability where
    arbitrary = Probability <$> (choose 0.0 1.0)
