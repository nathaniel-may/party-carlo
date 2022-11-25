-- | property testing for the project.
-- | currently there is no monadic quickcheck library for PureScript, so each test runs the test monad within the test
module Test.Property
    -- exporting only the full array to get dead code warnings if written tests aren't in the array
    (allTests)
    where

import Prelude

import Data.Foldable (sum)
import Data.Int (round)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Ord (abs)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import PartyCarlo.Capability.Random (class Random)
import PartyCarlo.Data.Probability (p999)
import PartyCarlo.Data.Probability as Prob
import PartyCarlo.MonteCarlo (Dist, monteCarloConfidenceInterval)
import PartyCarlo.Utils (count, randoms)
import Test.PropTestM (runPropTestM)
import Test.QuickCheck (Result, quickCheck, (<?>))
import Test.Utils (PracticalDist(..))


-- | array of all tests to run
allTests :: Array (Effect Unit)
allTests = [ quickCheck test0, quickCheck test1, quickCheck test2 ]

-- | for every distribution, the naive expected value should be contained within the Monte Carlo p999 confidence interval
test0 :: Dist -> Result
test0 dist = unsafePerformEffect $ runPropTestM test 
    where 
        test :: ∀ m. Random m => m Result
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
                    <> " rngSeed=NOT_AVAILABLE")

-- | for every generated set of 1000 probabilities, about half of them should be below 0.5
-- | useful as a smoke test for if a mistake is made when swapping out the rng
test1 :: Result
test1 = unsafePerformEffect $ runPropTestM test
    where 
        test :: ∀ m. Random m => m Result
        test = do
            let n = 1000
            let threshold = 0.1
            xs <- randoms n
            let lower = count (\x -> Prob.toNumber x < 0.5) xs
            let upper = n - lower
            let maxDiff = Int.toNumber n * threshold
            pure $ (Int.toNumber $ abs (lower - upper)) < maxDiff
                <?> ("random number generator is generating lop-sided sequences. Out of "
                <> show n <> " generated probabilities " 
                <> show lower <> " were < 0.5 and "
                <> show upper <> " were >= 0.5 which is above the threshold of a difference of "
                <> show maxDiff)

-- note that this property is impure because the random instance still relies on Effect.Random.random
test2 :: PracticalDist -> Result
test2 (PracticalDist dist) = unsafePerformEffect $ runPropTestM test 
    where 
        test :: ∀ m. Random m => m Result
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
                    <> " rngSeed=NOT_AVAILABLE")
