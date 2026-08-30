-- | module for every log line that the console batch API can output
module PartyCarlo.Api.Logs where

import Prelude

import Data.DateTime (DateTime, diff)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Time.Duration (Milliseconds)
import PartyCarlo.Api.Error (ApiError)
import PartyCarlo.Capability.LogMessages (class LogMessages, logWith)
import PartyCarlo.Capability.Now (class Now)
import PartyCarlo.Data.Display (class Display, display)
import PartyCarlo.Data.Log (LogLevel(..))


data ApiLog
    = ParsingCsv
    | ParsedGrid { labels :: Int, people :: Int }
    | ApiParsingFailed ApiError
    | RunningLabel { label :: String, index :: Int, total :: Int, experiments :: Int }
    | LabelDuration String DateTime DateTime
    | RunFailedLog String

derive instance genericApiLog :: Generic ApiLog _

instance showApiLog :: Show ApiLog where
    show = genericShow

instance displayApiLog :: Display ApiLog where
    display ParsingCsv =
        "parsing csv input."

    display (ParsedGrid r) =
        "parsed " <> display r.labels <> " columns of " <> display r.people <> " people."

    display (ApiParsingFailed e) =
        "csv parsing failed: " <> display e

    display (RunningLabel r) =
        "running " <> display r.experiments <> " experiments for column \"" <> r.label <> "\" (" <> display r.index <> " of " <> display r.total <> ")."

    display (LabelDuration l s e) =
        "column \"" <> l <> "\" calculated in " <> display (diff e s :: Milliseconds) <> "."

    display (RunFailedLog l) =
        "Monte Carlo confidence interval calculation failed for column \"" <> l <> "\"."

-- | user-input failures are surfaced to the caller as a rejected promise, so they log at Info.
-- | only an internal Monte Carlo failure is an Error.
logLevel :: ApiLog -> LogLevel
logLevel = case _ of
    ParsingCsv         -> Debug
    ParsedGrid _       -> Info
    ApiParsingFailed _ -> Info
    RunningLabel _     -> Info
    LabelDuration _ _ _ -> Info
    RunFailedLog _     -> Error

-- | Log a message with the log event's default level
log :: forall m. LogMessages ApiLog m => Now m => ApiLog -> m Unit
log = logWith logLevel
