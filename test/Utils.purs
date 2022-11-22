module Test.Utils where

import Prelude

import Data.Int (floor)
import PartyCarlo.MonteCarlo (Dist)
import PartyCarlo.Utils (replicateM)
import Random.PseudoRandom (Seed, mkSeed)
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (choose)


-- | wrapper type for generating rng seeds in prop tests
newtype SeedGen = SeedGen Seed

instance arbSeedGen :: Arbitrary SeedGen where
    arbitrary = SeedGen <<< mkSeed <<< floor <$> (choose 1.0 2147483647.0)

-- | wrapper type for generating the types of distributions users would input in prop tests
newtype PracticalDist = PracticalDist Dist

instance arbPracticalDist :: Arbitrary PracticalDist where
    arbitrary = do 
        n <- choose 5.0 80.0
        dist <- replicateM (floor n) arbitrary
        pure $ PracticalDist dist
