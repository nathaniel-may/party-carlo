-- | property testing for the project.
-- | currently there is no monadic quickcheck library for purescript, so each test runs the test monad within the test
module Test.Property where

import Prelude

import Data.Foldable (sum)
import Data.Int (round)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Ord (abs)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import PartyCarlo.Capability.Pack (class Pack)
import PartyCarlo.Data.Probability (p999)
import PartyCarlo.Data.Probability as Prob
import PartyCarlo.MonteCarlo (Dist, monteCarloConfidenceInterval)
import PartyCarlo.Utils (count, rngs)
import Random.PseudoRandom (Seed)
import Test.PropTestM (runPropTestM)
import Test.QuickCheck (Result, quickCheck, (<?>))
import Test.Utils (SeedGen(..), PracticalDist(..))


allTests :: Array (Effect Unit)
allTests = [ quickCheck test0, quickCheck test1, quickCheck test2 ]
    where

    test0 :: SeedGen -> Dist -> Result
    test0 (SeedGen seed) dist = runPropTestM test seed 
        where 
            test :: ∀ m. Pack Seed m => m Result
            test = do
                let expectedValue = round (sum $ Prob.toNumber <$> dist)
                -- testing with the smallest confidence interval that's used in the UI
                result <- monteCarloConfidenceInterval p999 5000 dist
                pure $ case result of
                    Nothing -> false <?> "confidenceInterval returned Nothing after running monte carlo methods in test0"
                    -- the expected value should be in the middle of the interval
                    Just ci@(Tuple low high) -> (low <= expectedValue && high >= expectedValue) 
                        <?> ("expected value was not inside the confidence interval: "
                        <> "ev=" <> show expectedValue 
                        <> " ci=" <> show ci
                        <> " dist=" <> (show $ Prob.toNumber <$> dist)
                        <> " rngSeed=" <> (show seed))

    -- | a smoke test for if a mistake is made when swapping out the rng
    test1 :: SeedGen -> Result
    test1 (SeedGen seed) = runPropTestM test seed 
        where 
            test :: ∀ m. Pack Seed m => m Result
            test = do
                let n = 1000
                let threshold = 0.1
                xs <- rngs n
                let lower = count (\x -> Prob.toNumber x < 0.5) xs
                let upper = n - lower
                let maxDiff = Int.toNumber n * threshold
                pure $ (Int.toNumber $ abs (lower - upper)) < maxDiff
                    <?> ("random number generator is generating lop-sided sequences. Out of "
                    <> show n <> " generated probabilities " 
                    <> show lower <> " were < 0.5 and "
                    <> show upper <> " were >= 0.5 which is above the threshold of a difference of "
                    <> show maxDiff)

    test2 :: SeedGen -> PracticalDist -> Result
    test2 (SeedGen seed) (PracticalDist dist) = runPropTestM test seed 
        where 
            test :: ∀ m. Pack Seed m => m Result
            test = do
                -- running less experiments is fine with a larger distribution
                result <- monteCarloConfidenceInterval p999 100 dist
                pure $ case result of
                    Nothing -> false <?> "confidenceInterval returned Nothing after running monte carlo methods in test2"
                    -- the two ends of the interval should not be the same
                    Just ci@(Tuple low high) -> (low /= high) 
                        <?> ("the two ends of the interval are the same when they should be different: "
                        <> " ci=" <> show ci
                        <> " dist=" <> (show $ Prob.toNumber <$> dist)
                        <> " rngSeed=" <> (show seed))
