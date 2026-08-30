-- | UI-free core of the application: input parsing and running experiments.
module PartyCarlo.Core where

import Prelude

import Data.Array (filter)
import Data.Either (Either(..), note)
import Data.Maybe (maybe)
import Data.Number as Number
import Data.String as String
import Data.String.Utils (lines)
import Data.Traversable (sequence)
import Effect.Aff.Class (class MonadAff, liftAff)
import PartyCarlo.Capability.Random (class Random)
import PartyCarlo.Core.Error (Error(..))
import PartyCarlo.Data.Probability (Probability, p90, p95, p99, p999, mkProbability)
import PartyCarlo.Data.Result (Result)
import PartyCarlo.Data.SortedArray as SortedArray
import PartyCarlo.MonteCarlo (confidenceInterval, parSample)
import PartyCarlo.Utils (mapLeft)


experimentCount :: Int
experimentCount = 100000

runExperiments :: ∀ m. MonadAff m => Random m => Int -> Array Probability -> m (Either Error Result)
runExperiments n dist = do
    samples <- liftAff $ parSample n dist
    let sorted = SortedArray.fromArray samples
    let result = (\p90val p95val p99val p999val ->
        { dist: sorted
        , p90: p90val
        , p95: p95val
        , p99: p99val
        , p999: p999val
        }) <$> confidenceInterval  p90  sorted
            <*> confidenceInterval p95  sorted
            <*> confidenceInterval p99  sorted
            <*> confidenceInterval p999 sorted
    pure $ note ExperimentsFailed result

parse :: Array String -> Either Error (Array Probability)
parse input = sequence $ (\s -> mapLeft (InvalidProbability s) <<< mkProbability =<< parseNum s) <$> input

parseNum :: String -> Either Error Number
parseNum s = maybe (Left $ InvalidNumber s) Right (Number.fromString s)

stripInput :: String -> Array String
stripInput s = filter (not String.null) $ String.trim <$> lines s
