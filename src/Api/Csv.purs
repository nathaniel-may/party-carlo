-- | csv parsing (via papaparse), grid validation, and csv rendering for the console batch API.
-- | everything but the papaparse call itself is pure so the rules are testable without JS.
module PartyCarlo.Api.Csv where

import Prelude

import Data.Array as Array
import Data.Either (Either(..), note)
import Data.Foldable (foldl, sum)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Data.String.Regex (Regex, test)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (mapAccumL, traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import PartyCarlo.Api.Error (ApiError(..))
import PartyCarlo.Api.Types (Column, IntervalRow)
import PartyCarlo.Core (parseNum) as Core
import PartyCarlo.Core.Error (Error(..)) as Core
import PartyCarlo.Data.Probability (mkProbability)
import PartyCarlo.Data.Probability as Prob


foreign import parseCsvImpl :: String -> { rows :: Array (Array String), errors :: Array String }

-- | true when the value really is a `Column`: JS callers can hand us anything
foreign import wellFormedColumn :: Column -> Boolean

-- | a short, safe rendering of an ill-formed column for the error message
foreign import describeColumn :: Column -> String

-- | parse pasted csv text into columns. any error surviving the FFI's non-fatal filter is fatal.
parseCsv :: String -> Either ApiError (Array Column)
parseCsv csv =
    let result = parseCsvImpl csv
    in case Array.head result.errors of
        Just e -> Left (CsvMalformed e)
        Nothing -> gridToColumns result.rows

-- | attach the 1-based number of the pasted line each row *starts* on. computed rather than
-- | inferred from the row index so dropped blank rows and quoted embedded newlines stay accurate.
annotate :: Array (Array String) -> Array { line :: Int, cells :: Array String }
annotate rows = (mapAccumL step 1 rows).value
    where
    step line cells =
        { accum: line + 1 + sum (map newlines cells)
        , value: { line, cells }
        }
    newlines cell = Array.length (String.split (String.Pattern "\n") cell) - 1

gridToColumns :: Array (Array String) -> Either ApiError (Array Column)
gridToColumns grid = do
    let rows = Array.filter (not <<< allBlank) (annotate grid)
    { head: hdr, tail: body } <- note NoHeader (Array.uncons rows)
    let labels = map String.trim hdr.cells
    checkBlanks labels
    checkDuplicates labels
    when (Array.null body) (Left NoDataRows)
    vs <- traverse (validateRow labels) body
    pure $ Array.mapWithIndex
        (\i label -> { label, probabilities: map (\row -> fromMaybe 0.0 (Array.index row i)) vs })
        labels
    where
    allBlank r = Array.all (String.null <<< String.trim) r.cells

-- | a blank label is a header artifact (usually a trailing comma), not a column anyone meant
checkBlanks :: Array String -> Either ApiError Unit
checkBlanks labels = case Array.findIndex String.null labels of
    Just i -> Left (BlankLabel (i + 1))
    Nothing -> Right unit

-- | first duplicate label in reading order fails the parse
checkDuplicates :: Array String -> Either ApiError Unit
checkDuplicates = void <<< foldl step (Right [])
    where
    step acc l = do
        seen <- acc
        if Array.elem l seen
            then Left (DuplicateLabel l)
            else Right (Array.snoc seen l)

-- | rows longer than the header are an error; shorter rows are padded with blanks (zeros)
validateRow :: Array String -> { line :: Int, cells :: Array String } -> Either ApiError (Array Number)
validateRow labels r =
    if actual > expected
    then Left (RowTooLong { line: r.line, expected, actual })
    else traverseWithIndex (cellValue labels r.line) (r.cells <> Array.replicate (expected - actual) "")
    where
    expected = Array.length labels
    actual = Array.length r.cells

-- | shape of a number the whole cell must match. `Core.parseNum` is `Number.fromString`, i.e.
-- | `parseFloat`, which happily consumes a numeric *prefix*: "0.5x", "0.9%" and the tab-separated
-- | cell "0.5\t0.9" (what a spreadsheet paste produces when the delimiter is not a comma) all
-- | parse as 0.5. The batch API must fail the entire run on any cell that is not a number, so the
-- | cell is shape-checked here first. `Core.parseNum` itself is unchanged: the textarea path keeps
-- | its historical leniency. See docs/adr/adr-002-strict-number-parsing.md.
numberShape :: Regex
numberShape = unsafeRegex "^[+-]?(?:[0-9]+\\.?[0-9]*|\\.[0-9]+)(?:[eE][+-]?[0-9]+)?$" noFlags

-- | `Core.parseNum`, but only for cells that are numbers end to end
strictParseNum :: String -> Either Core.Error Number
strictParseNum s =
    if test numberShape s
    then Core.parseNum s
    else Left (Core.InvalidNumber s)

cellValue :: Array String -> Int -> Int -> String -> Either ApiError Number
cellValue labels line i cell =
    if String.null trimmed
    then Right 0.0
    else case strictParseNum trimmed of
        Left e -> bad e
        Right n -> case mkProbability n of
            -- mkProbability returns the offending Number, not an Error, so build one here
            Left v -> bad (Core.InvalidProbability trimmed v)
            Right p -> Right (Prob.toNumber p)
    where
    trimmed = String.trim cell
    bad why = Left (BadCell { label: fromMaybe "" (Array.index labels i), line, cell: trimmed, why })

-- | re-validate a (possibly hand-edited) column. `index` is 0-based: an array has no line number.
validateColumn :: Column -> Either ApiError (Array Prob.Probability)
validateColumn col =
    if not (wellFormedColumn col)
    then Left (MalformedColumn (describeColumn col))
    else traverseWithIndex check col.probabilities
    where
    check i n = case mkProbability n of
        Left v -> Left (BadColumnValue { label: col.label, index: i, value: v, why: Core.InvalidProbability (show n) n })
        Right p -> Right p

validateExperiments :: Number -> Either ApiError Int
validateExperiments n = case Int.fromNumber n of
    Just i | i > 0 -> Right i
    _ -> Left (BadExperimentCount n)

toCsv :: Array IntervalRow -> String
toCsv rows = String.joinWith "\n" (Array.cons header (map line rows)) <> "\n"
    where
    header = ",p90_low,p90_high,p95_low,p95_high,p99_low,p99_high,p999_low,p999_high"
    -- `show`, not `display`: no thousands separators in machine-readable output
    line r = String.joinWith ","
        ( Array.cons (csvField r.label)
            (map show [ r.p90_low, r.p90_high, r.p95_low, r.p95_high, r.p99_low, r.p99_high, r.p999_low, r.p999_high ])
        )

csvField :: String -> String
csvField s =
    if Array.any (\c -> String.contains (String.Pattern c) s) [",", "\"", "\r", "\n"]
    then "\"" <> String.replaceAll (String.Pattern "\"") (String.Replacement "\"\"") s <> "\""
    else s
