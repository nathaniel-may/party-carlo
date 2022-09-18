module Main where

import Prelude

import Control.Monad.Error.Class (liftEither, class MonadError, throwError)
import Control.Monad.Except.Trans (runExceptT)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe, maybe)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log)
import MonteCarlo (confidenceInterval, sample)
import Probability (probability)
import SortedArray as SortedArray


main :: Effect Unit
main = do 
  result <- runExceptT app
  either log pure result

app :: ∀ m. Monad m => MonadError String m => MonadEffect m => m Unit
app = do
  let sampleSize = 1000000
  log $ "Running " <> show sampleSize <> " experiments..."
  let attendanceNumbers = [0.1, 0.9, 0.9, 0.99]  -- TODO dummy data
  attendanceDist <- either
    (throwError <<< probErr)
    pure
    (sequence $ probability <$> attendanceNumbers)
  samples <- liftEffect $ sample attendanceDist sampleSize
  log "sorting samples..."
  let sortedSamples = SortedArray.fromArray samples
  log "99.9% Confidence Interval for Attendance:"
  p999 <- either (throwError <<< probErr) pure (probability 0.999)
  result <- noteT "error computing confidence interval" (confidenceInterval p999 sortedSamples)
  log $ showTuple result

showTuple :: ∀ a b. Show a => Show b => Tuple a b -> String
showTuple (Tuple a b) = "(" <> show a <> ", " <> show b <> ")"

probErr :: Number -> String
probErr n = "error parsing distribution probabilities: " <> show n <> " is not between 0.0 and 1.0"

mapLeft :: ∀ a e' e. (e -> e') -> Either e a -> Either e' a
mapLeft f = either (Left <<< f) Right

note :: ∀ e a. e -> Maybe a -> Either e a
note e = maybe (Left e) Right

noteT :: ∀ e m a. MonadError e m => e -> Maybe a -> m a
noteT e = liftEither <<< note e
