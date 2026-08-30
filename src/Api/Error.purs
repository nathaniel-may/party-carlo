-- | error type for the console batch API
module PartyCarlo.Api.Error where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.Maybe (maybe)
import Data.Show.Generic (genericShow)
import PartyCarlo.Core.Error as Core
import PartyCarlo.Data.Display (class Display, display)


data ApiError
    = CsvMalformed String
    | NoHeader
    | NoDataRows
    | DuplicateLabel String
    | BlankLabel Int
    | MalformedColumn String
    | RowTooLong { line :: Int, expected :: Int, actual :: Int }
    | BadCell { label :: String, line :: Int, cell :: String, why :: Core.Error }
    | BadColumnValue { label :: String, index :: Int, value :: Number, why :: Core.Error }
    | BadExperimentCount Number
    | RunFailed String

derive instance eqApiError :: Eq ApiError
derive instance genericApiError :: Generic ApiError _

instance showApiError :: Show ApiError where
    show = genericShow

-- | string used to display the error value to the user (suitable for both UI and console logs)
-- | note: `show` rather than `display` for Int/Number, since `display` comma-formats integers.
instance displayApiError :: Display ApiError where
    display (CsvMalformed s) = "could not parse CSV: " <> s
    display NoHeader = "no header row: input is empty"
    display NoDataRows = "no data rows: input has a header but no people"
    display (DuplicateLabel l) = "duplicate column label \"" <> l <> "\""
    display (BlankLabel i) = "column " <> show i <> " has a blank label (a trailing comma in the header?)"
    display (MalformedColumn s) = "malformed column " <> s <> ": expected { label: string, probabilities: number[] }"
    display (RowTooLong r) = "line " <> show r.line <> ": row has " <> show r.actual <> " cells but the header has " <> show r.expected
    display (BadCell r) = "line " <> show r.line <> ", column \"" <> r.label <> "\": " <> display r.why
    display (BadColumnValue r) = "column \"" <> r.label <> "\", index " <> show r.index <> ": " <> display r.why
    -- render a whole number without the Number `show`'s trailing ".0"
    display (BadExperimentCount n) = "experiments must be a positive whole number, got " <> maybe (show n) show (Int.fromNumber n)
    display (RunFailed l) = "column \"" <> l <> "\": " <> display Core.ExperimentsFailed
