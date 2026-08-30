export function installImpl(api) {
  return function () {
    globalThis.partyCarlo = {
      parse:  (csv)       => api.parse(csv)(),
      run:    (cols, o)   => api.run(cols, o?.experiments ?? api.defaultExperiments)(),
      // Keep toCsv synchronous. Devtools' `copy` is in scope only while a console entry
      // evaluates synchronously - not alongside await-initialised declarations, and not
      // inside a .then callback - so `copy(partyCarlo.toCsv(rows))` has to be its own
      // entry over an already-resolved value (see the README snippet).
      toCsv:  (rows)      => api.toCsv(rows),
      runCsv: (csv, o)    => api.runCsv(csv, o?.experiments ?? api.defaultExperiments)(),
      defaultExperiments: api.defaultExperiments,
    };
  };
}
