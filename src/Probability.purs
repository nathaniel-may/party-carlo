module Probability (
    Probability
  , probability
  , toNumber
) where

import Prelude

import Data.Maybe (Maybe(..))
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck.Gen (choose)


data Probability = Probability Number

probability :: Number -> Maybe Probability
probability p = if p <= 1.0 && p >= 0.0 then Just (Probability p) else Nothing

toNumber :: Probability -> Number
toNumber (Probability x) = x

instance arbProbability :: Arbitrary Probability where
  arbitrary = Probability <$> (choose 0.0 1.0)
