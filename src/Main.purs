module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import MonteCarlo (confidenceInterval, sample)
import Probability (probability)
import SortedArray as SortedArray


main :: Effect Unit
main = do
  let sampleSize = 1000000
  log $ "Running " <> show sampleSize <> " experiments..."
  let attendanceNumbers = [0.01, 0.2, 0.2, 0.99]  -- TODO dummy data
  case sequence $ probability <$> attendanceNumbers of
    Nothing -> log "parsing error"
    Just attendanceDist -> do
      samples <- sample attendanceDist sampleSize
      log "sorting samples..."
      let sortedSamples = SortedArray.fromArray samples
      log "99.9% Confidence Interval for Attendance:"
      log case (\x -> confidenceInterval x sortedSamples) =<< probability 0.999 of
        Nothing -> "error"
        Just (Tuple a b) -> "(" <> show a <> ", " <> show b <> ")"
