import Papa from "papaparse";

// Non-fatal papaparse error classes. papaparse reports these on `errors` alongside perfectly
// usable `data`, so they must not fail the parse:
//   Delimiter     - "UndetectableDelimiter"; cannot occur now that `delimiter` is pinned, kept so a
//                   future papaparse cannot resurrect the bug below.
//   FieldMismatch - ragged rows; we detect those ourselves and report `RowTooLong` with a real
//                   1-based pasted line number.
const NON_FATAL = new Set(["Delimiter", "FieldMismatch"]);

export function parseCsvImpl(text) {
  const out = Papa.parse(text, { header: false, delimiter: "," });
  return {
    rows: out.data,
    errors: (out.errors ?? [])
      .filter((e) => !NON_FATAL.has(e.type))
      .map((e) => (e.row === undefined ? e.message : `${e.message} (near row ${e.row + 1})`)),
  };
}

export function wellFormedColumn(col) {
  return (
    col !== null &&
    typeof col === "object" &&
    typeof col.label === "string" &&
    Array.isArray(col.probabilities) &&
    col.probabilities.every((p) => typeof p === "number")
  );
}

export function describeColumn(col) {
  let s;
  try {
    s = JSON.stringify(col);
  } catch (_) {
    s = undefined;
  }
  if (s === undefined) s = String(col);
  return s.length > 60 ? s.slice(0, 60) + "..." : s;
}
