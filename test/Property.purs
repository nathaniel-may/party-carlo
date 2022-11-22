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
import PartyCarlo.Data.Probability (Probability)
import PartyCarlo.Data.Probability as Prob
import PartyCarlo.MonteCarlo (Dist, monteCarloConfidenceInterval)
import PartyCarlo.Utils (count, rngs)
import Random.PseudoRandom (Seed)
import Test.PropTestM (runPropTestM)
import Test.QuickCheck (Result, quickCheck, (<?>))
import Test.Utils (SeedGen(..))


allTests :: Array (Effect Unit)
allTests = [ quickCheck test0, quickCheck test1 ]
    where

    test0 :: SeedGen -> Probability -> Dist -> Result
    test0 (SeedGen seed) p dist = runPropTestM test seed 
        where 
            test :: ∀ m. Pack Seed m => m Result
            test = do
                let expectedValue = round (sum $ Prob.toNumber <$> dist)
                -- if the number of runs is too low, the confidence interval is smaller than is should be
                -- which causes this test to fail. from previous failing tests, 5000 is too small.
                result <- monteCarloConfidenceInterval p 10000 dist
                pure $ case result of
                    -- no values means the requested confidence interval was <= 0.5
                    -- which would have otherwise flipped low > high values
                    Nothing -> Prob.toNumber p <= 0.5 <?> "p > 0.5 but confidenceInterval returned Nothing."
                    -- the expected value should be in the middle of the interval
                    Just ci@(Tuple low high) -> (low <= expectedValue && high >= expectedValue) 
                        <?> ("expected value was not inside the confidence interval: "
                        <> "ev=" <> show expectedValue 
                        <> " ci=" <> show ci
                        <> " p=" <> (show $ Prob.toNumber p)
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

