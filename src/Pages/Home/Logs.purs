-- | module for every log line that the home page can output
module PartyCarlo.Pages.Home.Logs where

import Prelude

import Data.Array as Array
import Data.DateTime (DateTime, diff)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Time.Duration (Milliseconds)
import PartyCarlo.Capability.LogMessages (class LogMessages, logMessage)
import PartyCarlo.Capability.Now (class Now)
import PartyCarlo.Data.Display (class Display, display)
import PartyCarlo.Data.Log (LogLevel(..), mkLog)
import PartyCarlo.Data.Result (Result)
import PartyCarlo.MonteCarlo (Dist)
import PartyCarlo.Pages.Home.Error (Error)


data HomeLog
    = PartyCarloButtonPressed HomeStateType
    | Transition HomeStateType HomeStateType
    | RunAction
    | ParsingFailed Error
    | ParsedNProbabilities Dist
    | Distribution Dist
    | RunningNExperiments Int
    | MonteCarloFailed
    | CalculationDuration DateTime DateTime
    | Intervals Result

derive instance genericHomeLog :: Generic HomeLog _

instance showHomeLog :: Show HomeLog where
    show = genericShow

instance displayHomeLog :: Display HomeLog where
    display (PartyCarloButtonPressed state) =
        "party carlo pressed in " <> display state <> " view."

    display (Transition from to) =
        "changing view from " <> display from <> " to " <> display to <> "."

    display RunAction =
        "run action initiated."

    display (ParsingFailed e) =
        "parsing failed: " <> display e
    
    display (ParsedNProbabilities dist) =
        "parsed probabilities for " <> (display $ Array.length dist) <> " attendees."

    display (Distribution dist) =
        "parsed distribution: " <> display dist

    display (RunningNExperiments n) =
        "running " <> display n <> " experiments."
    
    display MonteCarloFailed =
        "Monte Carlo confidence interval calculation failed."

    display (CalculationDuration start end) =
        "result calculated in " <> display (diff end start :: Milliseconds) <> "."

    display (Intervals r) =
        Array.fold ["result set: p90=", display r.p90, " p95=", display r.p95, " p99=", display r.p99, " p99.9=", display r.p999]

data HomeStateType
    = DataState
    | ResultsState
    | LoadingState

derive instance genericHomeStateType :: Generic HomeStateType _

instance showHomeStateType :: Show HomeStateType where
    show = genericShow

instance displayHomeStateType :: Display HomeStateType where
    display DataState    = "data"
    display ResultsState = "results"
    display LoadingState = "loading"

logLevel :: HomeLog -> LogLevel
logLevel = case _ of
    PartyCarloButtonPressed _ -> Debug
    Transition _ _            -> Debug
    RunAction                 -> Debug
    ParsingFailed _           -> Info
    ParsedNProbabilities _    -> Info
    Distribution _            -> Debug
    RunningNExperiments _     -> Info
    MonteCarloFailed          -> Error
    CalculationDuration _ _   -> Info
    Intervals _               -> Debug

-- | Log a message with a level
logWith :: forall m log. LogMessages log m => Now m => (log -> LogLevel) -> log -> m Unit
logWith f l = logMessage <=< mkLog (f l) $ l

-- | Log a message with the log event's default level
log :: forall m. LogMessages HomeLog m => Now m => HomeLog -> m Unit
log = logWith logLevel
