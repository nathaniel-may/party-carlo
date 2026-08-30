# party-carlo
It's easy to assign the likelihood you think one person will attend a party, but it's harder to reason about a collection of probabilities to represent a group. This client-side web application will use your individual probabilites with Monte Carlo methods to compose them into a distribution that can be used to reason about the group as a whole with confidence intervals.

## Local Dev
```
npm install
npm run ps-install
npm run build
```
then open `./index.html` in a browser

To run the tests:
```
npm install
bash scripts/select-env.sh
spago build
spago test
```
`npm install` must precede `spago test` because the CSV tests call papaparse, and
`scripts/select-env.sh` generates the gitignored `src/Env.purs` that `spago build` needs.

## Console batch API

The deployed page installs `globalThis.partyCarlo`, so a whole spreadsheet of columns can be run
from the browser console without a local build. Paste a grid of columns (one column per date), get
back a CSV of confidence intervals per column.

One shot:
```js
const csv = `2025-08-14,2025-08-15
0.9,0.8
0.3,0.5`;
copy(await partyCarlo.runCsv(csv));
```

Three stages, so a mistyped cell can be inspected and fixed in the devtools tree. Note the `await`
on `parse`: it returns a `Promise` even though it does no asynchronous work, so that it logs its
progress like the other calls.
```js
const cols = await partyCarlo.parse(csv);   // array of { label, probabilities } - inspect/edit here
const rows = await partyCarlo.run(cols);
```

Then, as a **separate** console entry:
```js
copy(partyCarlo.toCsv(rows));
```

`run` and `runCsv` take an options object: `partyCarlo.run(cols, { experiments: 1000000 })`. The
default is `partyCarlo.defaultExperiments`, the same count the page's button uses.

Input format:
- cells are separated by **commas, not tabs** - a tab-separated paste is rejected rather than
  silently misread;
- the header row is the column labels, treated as opaque strings and passed through unchanged;
- every later row is one person, one cell per label - there is **no name column**;
- a blank cell is `0`;
- all-blank rows are dropped (an all-zero person contributes nothing to the model);
- rows longer than the header, duplicate labels and blank labels (usually a trailing comma in the
  header) are errors;
- any cell that is not a number in `[0,1]` end to end fails the whole run - a partly-numeric cell
  such as `0.9%` is an error, not `0.9` - reported with the column label and the 1-based line
  number as pasted.

Output format: one row per label in input column order, the first cell of each row is the label,
and the header row's first cell is empty.

```
,p90_low,p90_high,p95_low,p95_high,p99_low,p99_high,p999_low,p999_high
2025-08-14,31,48,29,51,25,55,21,60
```

## Future Improvements
- Better cross-device screen sizing. Right now phones smaller than mine have to scroll around to see the full app.
