module Test.Utils where

import Prelude

import Data.Int (floor)
import Data.Tuple (Tuple(..))
import Random.PseudoRandom (Seed, mkSeed)
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck.Gen (choose)


-- | wrapper type for generating rng seeds in prop tests
newtype SeedGen = SeedGen Seed

instance arbSeedGen :: Arbitrary SeedGen where
    arbitrary = SeedGen <<< mkSeed <<< floor <$> (choose 1.0 2147483647.0)
