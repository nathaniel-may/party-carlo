-- | Home page error type
module PartyCarlo.Pages.Home.Error where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import PartyCarlo.Data.Display (class Display)
import PartyCarlo.Utils (displayTrunc)


-- TODO add another layer of heirarchy so it's clear which errors are being passed
data Error
    = InvalidNumber String
    | InvalidProbability String Number
    | ExperimentsFailed

derive instance eqError :: Eq Error
derive instance genericError :: Generic Error _

instance showError :: Show Error where
    show = genericShow

-- | string used to display the error value to the user (suitable for both UI and console logs)
instance displayError :: Display Error where
    display (InvalidNumber s) = "\"" <> displayTrunc 13 s <> "\"" <> " is not a number"
    display (InvalidProbability s _) = displayTrunc 13 s <> " is not a probability (between 0 and 1)"
    display ExperimentsFailed = "experiments failed to run. copy your data, reload the page, and try again."
