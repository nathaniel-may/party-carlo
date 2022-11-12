module Test.Main (main) where

import Prelude

import Data.Foldable (sum)
import Data.Int (round)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import PartyCarlo.MonteCarlo (Dist, monteCarloConfidenceInterval)
import PartyCarlo.Data.Probability (Probability)
import PartyCarlo.Data.Probability as Prob
import Test.QuickCheck (Result, quickCheck, (<?>))


main :: Effect Unit
main = void $ sequence allTests

allTests :: Array (Effect Unit)
allTests = [
    quickCheck test0
  ]

test0' :: Probability -> Dist -> Effect Result
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
test0 p dist = unsafePerformEffect (test0' p dist)

showTuple :: ∀ a b. Show a => Show b => Tuple a b -> String
showTuple (Tuple a b) = "(" <> show a <> ", " <> show b <> ")"
