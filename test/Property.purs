module Test.Property where

import Prelude

import Data.Foldable (sum)
import Data.Int (round)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import PartyCarlo.Capability.RNG (class RNG)
import PartyCarlo.Data.Probability (Probability)
import PartyCarlo.Data.Probability as Prob
import PartyCarlo.MonteCarlo (Dist, monteCarloConfidenceInterval)
import Test.QuickCheck (Result, quickCheck, (<?>))
import Test.TestM (runTestM, initialState)
import Test.Utils (showTuple)


allTests :: Array (Effect Unit)
allTests = [ quickCheck test0 ]
    where

    test0' :: ∀ m. RNG m => Probability -> Dist -> m Result
    test0' p dist = do
        let expectedValue = round (sum $ Prob.toNumber <$> dist)
        result <- monteCarloConfidenceInterval p 5000 dist
        pure $ case result of
            -- no values means the requested confidence interval was <= 0.5
            -- which would have otherwise flipped low > high values
            Nothing -> Prob.toNumber p <= 0.5 <?> "p > 0.5 but confidenceInterval returned Nothing."
            -- the expected value should be in the middle of the interval
            Just ci@(Tuple low high) -> (low <= expectedValue && high >= expectedValue) 
                <?> ("expected value was not inside the confidence interval: "
                <> "ev=" <> show expectedValue 
                <> " ci=" <> showTuple ci
                <> " p=" <> (show $ Prob.toNumber p)
                <> " dist=" <> (show $ Prob.toNumber <$> dist))

    -- dirty way to force the effect before passing to quickcheck
    test0 :: Probability -> Dist -> Result
    test0 p dist = unsafePerformEffect $ runTestM initialState (test0' p dist) 
