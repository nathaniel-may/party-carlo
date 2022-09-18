module Main where

import Prelude

import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Monad.Except.Trans (runExceptT)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
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
  case result of
    Left msg -> log msg
    Right _ -> pure unit

app :: ∀ m. Monad m => MonadError String m => MonadEffect m => m Unit
app = do
  let sampleSize = 1000000
  log $ "Running " <> show sampleSize <> " experiments..."
  let attendanceNumbers = [0.01, 0.2, 0.2, 0.99]  -- TODO dummy data
  attendanceDist <- case sequence $ probability <$> attendanceNumbers of
    Nothing -> throwError "error parsing distribution probabilities"
    Just a -> pure a
  samples <- liftEffect $ sample attendanceDist sampleSize
  log "sorting samples..."
  let sortedSamples = SortedArray.fromArray samples
  log "99.9% Confidence Interval for Attendance:"
  result <- case (\x -> confidenceInterval x sortedSamples) =<< probability 0.999 of
    Nothing -> throwError "error calculating the confidence interval"
    Just a -> pure a
  log $ showTuple result

showTuple :: ∀ a b. Show a => Show b => Tuple a b -> String
showTuple (Tuple a b) = "(" <> show a <> ", " <> show b <> ")"
