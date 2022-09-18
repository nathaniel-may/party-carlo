module Probability (
    Probability
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

instance arbProbability :: Arbitrary Probability where
  arbitrary = Probability <$> (choose 0.0 1.0)
