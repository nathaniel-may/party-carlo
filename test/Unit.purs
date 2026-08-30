-- | module for all unit tests
module Test.Unit
    -- exporting only the full array to get dead code warnings if written tests aren't in the array
    (allTests) 
    where

import Prelude

import Control.Monad.State.Class (class MonadState, get)
import Data.Array (length)
import Data.Array as Array
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect.Class (class MonadEffect, liftEffect)
import PartyCarlo.Capability.LogMessages (class LogMessages)
import PartyCarlo.Capability.Now (class Now)
import PartyCarlo.Capability.Random (class Random)
import PartyCarlo.Capability.Sleep (class Sleep)
import PartyCarlo.Api.Csv as Csv
import PartyCarlo.Api.Error (ApiError(..))
import PartyCarlo.Core as Core
import PartyCarlo.Core.Error (Error(..))
import PartyCarlo.Data.Log (Log, LogLevel(..), level, vals)
import PartyCarlo.Data.Probability (p95, mkProbability)
import PartyCarlo.MonteCarlo (monteCarloConfidenceInterval)
import PartyCarlo.Pages.Home (State(..))
import PartyCarlo.Pages.Home as Home
import PartyCarlo.Pages.Home.Logs (HomeLog(..))
import Test.Capability.Assert (class Assert, assert, assertEqual, fail)
import Test.Capability.Metadata (class Metadata, getMeta)
import Test.TestM as TestM


-- | array of all tests to run
allTests
    :: forall m
    . Assert m
    => Metadata TestM.Meta m
    => Sleep m
    => LogMessages HomeLog m 
    => Now m
    => Random m
    => MonadState Home.State m
    => Array (m Unit)
allTests =
    [ test0, test1, test2, test3, test4
    , csvHappyPath
    , csvQuoting
    , csvCrlf
    , csvShortRowPadded
    , csvAllBlankRowDropped
    , csvLineNumberAfterBlankRow
    , csvLineNumberAfterEmbeddedNewline
    , csvRowTooLong
    , csvDuplicateLabel
    , csvNonNumericCell
    , csvNumericPrefixCell
    , csvTabDelimitedPaste
    , csvBlankLabel
    , csvOutOfRangeCell
    , csvEmptyInput
    , csvUnterminatedQuote
    , csvHeaderOnly
    , validateColumnTest
    , validateExperimentsTest
    , toCsvTest
    , toCsvQuotingTest
    ]

failOnLog :: forall m. Assert m => Metadata TestM.Meta m => (Log HomeLog -> Boolean) -> m Unit
failOnLog f = getMeta >>= \run -> maybe
    (pure unit)
    (\log -> fail $ "Error log found during test: " <> show log)
    (Array.find f run.logs)

-- | test that time and logging effects are taking place a reasonable amount of times within the component's handleAction function
test0
    :: forall m
    . Assert m
    => Metadata TestM.Meta m
    => Sleep m
    => LogMessages HomeLog m 
    => Now m
    => Random m
    => MonadState Home.State m
    => m Unit
test0 = do
    -- press the button with well formed input
    Home.handleAction' (Home.Data { e : Nothing, input : ".5" }) Home.ButtonPress
    s <- getMeta
    -- assert that the log event which reports how long the calculation took was sent
    assert "calculation duration should be logged" (
        Array.any (\log -> case vals log of
            CalculationDuration _ _ -> true
            _ -> false) s.logs
        )
    -- assert time effect and log effects happened the correct number of times
    assertEqual "the result should have been timed, so time should have been accessed two more times than the number of logs." { actual: s.timeCounter - (length s.logs), expected: 2 }
    assert "logged less than expected" (length s.logs >= 6)
    -- fail if any logs have the error level
    failOnLog \log -> level log == Error

-- | test that the monte carlo library returns confidence intervals of size > 0.
-- | this is useful for catching problems with the rng or seed storage
test1
    :: forall m
    . Assert m
    => MonadEffect m
    => m Unit
test1 = case traverse (hush <<< mkProbability) [0.1, 0.99, 0.5, 0.5] of
    Nothing -> 
        fail "probabilies failed to parse in test1"
    Just dist ->
        liftEffect (monteCarloConfidenceInterval p95 Core.experimentCount dist) >>= case _ of
            Nothing -> fail "monte carlo methods failed for test1"
            Just (Tuple low high) -> assert 
                "the size of the p95 confidence interval for the default input was zero" 
                (low /= high)

-- | test that the monte carlo library returns confidence intervals of size > 0 but within the handleAction function.
-- | this is useful for catching problems with the rng or seed storage
test2
    :: forall m
    . Assert m
    => Metadata TestM.Meta m
    => Sleep m
    => LogMessages HomeLog m 
    => Now m
    => Random m
    => MonadState Home.State m
    => m Unit
test2 = do
    -- press the button with well formed input
    Home.handleAction' (Home.Data { e : Nothing, input : ".1\n.99\n.5\n.5\n" }) Home.ButtonPress
    s <- get
    case s of
        Results { input: _, result: r } -> case r.p95 of
            Tuple x y -> assert "the confidence interval for the default input should not be of size zero" (x /= y)
        _ -> assert "while testing the result of pressing the button, an unexpected state change occurred." false
    -- fail if any logs have the error level
    failOnLog \log -> level log == Error

-- | test that bad input is handled properly when the button is pressed the component's handleAction function
test3
    :: forall m
    . Assert m
    => Metadata TestM.Meta m
    => Sleep m
    => LogMessages HomeLog m 
    => Now m
    => Random m
    => MonadState Home.State m
    => m Unit
test3 = do
    let badInput = "bad input"
    Home.handleAction' (Home.Data { e : Nothing, input : badInput }) Home.ButtonPress
    get >>= case _ of
        Data state ->
            assert "pressing the button with a bad non-Number input should yeild an InvalidNumber error" (state.e == Just (InvalidNumber badInput))
        Loading ->
            fail $ "pressing the button with a bad input yielded the invalid state `Loading` in test 3"
        Results _ ->
            fail $ "pressing the button with a bad input yielded the invalid state `Results` in test 3"
    -- fail if any logs have the error level
    failOnLog \log -> level log == Error
    
-- | test that bad input is handled properly when the button is pressed the component's handleAction function
test4
    :: forall m
    . Assert m
    => Metadata TestM.Meta m
    => Sleep m
    => LogMessages HomeLog m 
    => Now m
    => Random m
    => MonadState Home.State m
    => m Unit
test4 = do
    let badInput = "2"
    Home.handleAction' (Home.Data { e : Nothing, input : badInput }) Home.ButtonPress
    get >>= case _ of
        Data state ->
            assert "pressing the button with a bad Number input should yeild an InvalidProbability error" (state.e == Just (InvalidProbability badInput 2.0))
        Loading ->
            fail "pressing the button with a bad input yielded the invalid state `Loading` in test 4"
        Results _ ->
            fail "pressing the button with a bad input yielded the invalid state `Results` in test 4"
    -- fail if any logs have the error level
    failOnLog \log -> level log == Error

--------------------------------------------------------------------------------
-- console batch API: csv parsing, validation and rendering (all pure)         --
--------------------------------------------------------------------------------

-- | csv text cases go through Csv.parseCsv, which exercises real papaparse. if a broad swathe
-- | of these fail at once with a CsvMalformed about delimiters, the bug is in src/Api/Csv.js's
-- | options, not in the PureScript.

-- | blank cell becomes 0 and the trailing newline row is dropped
csvHappyPath :: forall m. Assert m => m Unit
csvHappyPath = assertEqual "csv happy path"
    { actual: Csv.parseCsv "a,b\n0.5,1\n,0.25\n"
    , expected: Right
        [ { label: "a", probabilities: [0.5, 0.0] }
        , { label: "b", probabilities: [1.0, 0.25] }
        ]
    }

csvQuoting :: forall m. Assert m => m Unit
csvQuoting = assertEqual "csv quoted fields"
    { actual: Csv.parseCsv "\"a,1\",b\n0.5,\"0.25\"\n"
    , expected: Right
        [ { label: "a,1", probabilities: [0.5] }
        , { label: "b", probabilities: [0.25] }
        ]
    }

csvCrlf :: forall m. Assert m => m Unit
csvCrlf = assertEqual "csv with CRLF line endings"
    { actual: Csv.parseCsv "a,b\r\n0.5,1\r\n,0.25\r\n"
    , expected: Right
        [ { label: "a", probabilities: [0.5, 0.0] }
        , { label: "b", probabilities: [1.0, 0.25] }
        ]
    }

csvShortRowPadded :: forall m. Assert m => m Unit
csvShortRowPadded = assertEqual "rows shorter than the header are padded with zeros"
    { actual: Csv.parseCsv "a,b\n0.5\n"
    , expected: Right
        [ { label: "a", probabilities: [0.5] }
        , { label: "b", probabilities: [0.0] }
        ]
    }

csvAllBlankRowDropped :: forall m. Assert m => m Unit
csvAllBlankRowDropped = assertEqual "all-blank rows are dropped"
    { actual: Csv.parseCsv "a,b\n0.5,0.5\n,\n0.25,0.25\n"
    , expected: Right
        [ { label: "a", probabilities: [0.5, 0.25] }
        , { label: "b", probabilities: [0.5, 0.25] }
        ]
    }

-- | the bad cell is on pasted line 4 even though it is the third surviving row: the all-blank
-- | line 3 is dropped after line numbers are attached.
csvLineNumberAfterBlankRow :: forall m. Assert m => m Unit
csvLineNumberAfterBlankRow = assertEqual "line number is correct after a dropped blank row"
    { actual: Csv.parseCsv "a,b\n0.5,0.5\n,\n0.25,9\n"
    , expected: Left (BadCell { label: "b", line: 4, cell: "9", why: InvalidProbability "9" 9.0 })
    }

-- | the header's first field is quoted and contains a newline, so the header occupies pasted
-- | lines 1-2 and the bad cell is on line 4. a naive 0-based data-row-index + 2 would say 3.
csvLineNumberAfterEmbeddedNewline :: forall m. Assert m => m Unit
csvLineNumberAfterEmbeddedNewline = assertEqual "line number is correct after an embedded newline"
    { actual: Csv.parseCsv "\"a\nz\",b\n0.5,0.5\n0.25,9\n"
    , expected: Left (BadCell { label: "b", line: 4, cell: "9", why: InvalidProbability "9" 9.0 })
    }

csvRowTooLong :: forall m. Assert m => m Unit
csvRowTooLong = assertEqual "rows longer than the header are an error"
    { actual: Csv.parseCsv "a,b\n1,2,3\n"
    , expected: Left (RowTooLong { line: 2, expected: 2, actual: 3 })
    }

csvDuplicateLabel :: forall m. Assert m => m Unit
csvDuplicateLabel = assertEqual "duplicate column labels are an error"
    { actual: Csv.parseCsv "a,a\n0.5,0.5\n"
    , expected: Left (DuplicateLabel "a")
    }

csvNonNumericCell :: forall m. Assert m => m Unit
csvNonNumericCell = assertEqual "non-numeric cells are an error"
    { actual: Csv.parseCsv "a\nx\n"
    , expected: Left (BadCell { label: "a", line: 2, cell: "x", why: InvalidNumber "x" })
    }

csvOutOfRangeCell :: forall m. Assert m => m Unit
csvOutOfRangeCell = assertEqual "cells outside [0,1] are an error"
    { actual: Csv.parseCsv "a\n1.1\n"
    , expected: Left (BadCell { label: "a", line: 2, cell: "1.1", why: InvalidProbability "1.1" 1.1 })
    }

-- | Number.fromString is parseFloat, which consumes a numeric *prefix*; the batch API must not
-- | silently accept "0.5x" as 0.5
csvNumericPrefixCell :: forall m. Assert m => m Unit
csvNumericPrefixCell = assertEqual "cells that are only partly a number are an error"
    { actual: Csv.parseCsv "a\n0.5x\n"
    , expected: Left (BadCell { label: "a", line: 2, cell: "0.5x", why: InvalidNumber "0.5x" })
    }

-- | a tab-separated spreadsheet paste is one wide column; it must fail loudly rather than
-- | silently keeping the numeric prefix of each row and dropping the rest
csvTabDelimitedPaste :: forall m. Assert m => m Unit
csvTabDelimitedPaste = assertEqual "a tab-delimited paste is rejected"
    { actual: Csv.parseCsv "a\tb\n0.5\t0.9\n"
    , expected: Left (BadCell { label: "a\tb", line: 2, cell: "0.5\t0.9", why: InvalidNumber "0.5\t0.9" })
    }

-- | a trailing comma in the header would otherwise yield a phantom ""-labelled column
csvBlankLabel :: forall m. Assert m => m Unit
csvBlankLabel = assertEqual "blank column labels are an error"
    { actual: Csv.parseCsv "a,b,\n0.5,0.5,0.5\n"
    , expected: Left (BlankLabel 3)
    }

csvEmptyInput :: forall m. Assert m => m Unit
csvEmptyInput = assertEqual "empty input has no header"
    { actual: Csv.parseCsv ""
    , expected: Left NoHeader
    }

-- | the only case that reaches CsvMalformed: a papaparse error that survives the FFI's
-- | non-fatal filter. if papaparse's wording drifts on a minor upgrade, this is the intended
-- | breakage point.
csvUnterminatedQuote :: forall m. Assert m => m Unit
csvUnterminatedQuote = assertEqual "unterminated quotes are a fatal papaparse error"
    { actual: Csv.parseCsv "a,b\n\"0.5,1\n"
    , expected: Left (CsvMalformed "Quoted field unterminated (near row 2)")
    }

csvHeaderOnly :: forall m. Assert m => m Unit
csvHeaderOnly = assertEqual "a header with no people is an error"
    { actual: Csv.parseCsv "a,b\n"
    , expected: Left NoDataRows
    }

-- | a hand-edited column is re-validated; index is 0-based since an array has no line number
validateColumnTest :: forall m. Assert m => m Unit
validateColumnTest = assertEqual "hand-edited columns are re-validated"
    { actual: Csv.validateColumn { label: "a", probabilities: [0.5, 1.1] }
    , expected: Left (BadColumnValue { label: "a", index: 1, value: 1.1, why: InvalidProbability "1.1" 1.1 })
    }

validateExperimentsTest :: forall m. Assert m => m Unit
validateExperimentsTest = do
    assertEqual "a positive whole experiment count is accepted"
        { actual: Csv.validateExperiments 1000.0, expected: Right 1000 }
    assertEqual "a zero experiment count is rejected"
        { actual: Csv.validateExperiments 0.0, expected: Left (BadExperimentCount 0.0) }
    assertEqual "a fractional experiment count is rejected"
        { actual: Csv.validateExperiments 1.5, expected: Left (BadExperimentCount 1.5) }

-- | asserts no thousands separators in the rendered output
toCsvTest :: forall m. Assert m => m Unit
toCsvTest = assertEqual "toCsv renders the header and one row per label"
    { actual: Csv.toCsv
        [ { label: "2025-08-14"
          , p90_low: 31, p90_high: 48
          , p95_low: 29, p95_high: 51
          , p99_low: 25, p99_high: 55
          , p999_low: 21, p999_high: 60
          }
        ]
    , expected: ",p90_low,p90_high,p95_low,p95_high,p99_low,p99_high,p999_low,p999_high\n2025-08-14,31,48,29,51,25,55,21,60\n"
    }

toCsvQuotingTest :: forall m. Assert m => m Unit
toCsvQuotingTest = assertEqual "toCsv quotes labels containing commas or quotes"
    { actual: Csv.toCsv
        [ { label: "a,b"
          , p90_low: 1, p90_high: 2, p95_low: 3, p95_high: 4
          , p99_low: 5, p99_high: 6, p999_low: 7, p999_high: 8
          }
        , { label: "say \"hi\""
          , p90_low: 1, p90_high: 2, p95_low: 3, p95_high: 4
          , p99_low: 5, p99_high: 6, p999_low: 7, p999_high: 8
          }
        ]
    , expected: ",p90_low,p90_high,p95_low,p95_high,p99_low,p99_high,p999_low,p999_high\n\"a,b\",1,2,3,4,5,6,7,8\n\"say \"\"hi\"\"\",1,2,3,4,5,6,7,8\n"
    }
