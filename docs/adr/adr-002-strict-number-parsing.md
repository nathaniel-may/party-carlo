# Strict number parsing in the batch API

## Context
`PartyCarlo.Core.parseNum` is `Data.Number.fromString`, whose FFI is `parseFloat` guarded by
`isFinite`. `parseFloat` consumes a numeric *prefix* and ignores the rest of the string, so
`"0.5x"`, `"0.9%"` and `"0.5 0.7"` all parse as `0.5`, and `"1,5"` parses as `1.0` — a valid
probability, so nothing downstream rejects it. The textarea has always behaved this way: one
value per line, visible on screen, with the parsed count logged back to the user.

The console batch API changes what that leniency costs. Input is a whole spreadsheet grid
pasted at once, output is a CSV pasted straight back into a sheet, and nobody reads the
intermediate numbers. The likely trigger is not a typo but a tab-separated paste, which is what
Cmd-C out of Excel or Sheets produces: with the papaparse delimiter pinned to `,`, the row
`"0.5\t0.9"` is one cell that `parseFloat` reduces to `0.5`. The result is a single phantom
column, every row's later values silently discarded, and a confident wrong interval CSV with no
diagnostic anywhere.

## Decision
Shape-check the whole cell before parsing it, in `PartyCarlo.Api.Csv` only. A trimmed cell must
match `^[+-]?(?:[0-9]+\.?[0-9]*|\.[0-9]+)(?:[eE][+-]?[0-9]+)?$` end to end before it is handed to
`Core.parseNum`; anything else fails the run with the existing `BadCell`/`InvalidNumber`, naming
the column label and the 1-based pasted line number. `Core.parseNum` is unchanged, so the
textarea keeps its historical behaviour.
    - Pros:
        - Meets the batch API's stated rule that any cell which is not a number in `[0, 1]` fails
          the entire run, for the whole class of partly-numeric cells rather than just the
          obviously non-numeric ones.
        - A tab-separated paste — the most likely real mistake — fails loudly on line 2 instead of
          producing a plausible wrong answer.
        - The UI's behaviour is untouched, so this cannot regress the page.
    - Cons:
        - Two parsing strictnesses now exist for the same notion of "a number", which reads as an
          inconsistency until you know why.
        - The shape language is ours to maintain: a legitimate spreadsheet rendering it does not
          admit would be a false rejection.

## Alternatives
- Tighten `Core.parseNum` itself so both paths are strict.
    - Pros:
        - One rule, no divergence to explain.
    - Cons:
        - Changes long-standing UI behaviour as a side effect of adding a console feature. Someone
          whose textarea input has always been accepted would start seeing errors, and this PR has
          no mandate for that.
- Accept the leniency in the batch API and document it.
    - Pros:
        - No new code; the two paths agree.
    - Cons:
        - The failure is silent and the output is trusted. For a tool whose entire value is that
          the numbers can be pasted into a sheet unchecked, a wrong answer is worse than a
          rejected one.
- Detect tabs specifically, or auto-detect the delimiter.
    - Pros:
        - Fixes the most likely trigger directly, and could even accept a tab-separated paste.
    - Cons:
        - Only addresses one symptom; `0.9%` and `1,5` still slip through. Auto-detection was also
          the source of a separate papaparse bug this feature already had to pin around.

## Status
Implemented in `PartyCarlo.Api.Csv` (`numberShape`/`strictParseNum`), with `csvNumericPrefixCell`
and `csvTabDelimitedPaste` in `test/Unit.purs` covering a numeric suffix and a whole tab-delimited
paste.

## Consequences
- The batch API and the textarea disagree about what counts as a number, deliberately. Unifying
  them on `Core.parseNum` reintroduces the silent-wrong-CSV failure; `csvNumericPrefixCell` and
  `csvTabDelimitedPaste` are what fail if someone tries.
- Exponent and leading-dot forms (`5e-1`, `.25`) are admitted; a spreadsheet rendering outside the
  shape would be a false rejection, and the fix is to widen the shape rather than to fall back to
  `parseFloat`.
