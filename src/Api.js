export function installImpl(api) {
  return function () {
    globalThis.partyCarlo = {
      parse:  (csv)       => api.parse(csv)(),
      run:    (cols, o)   => api.run(cols, o?.experiments ?? api.defaultExperiments)(),
      toCsv:  (rows)      => api.toCsv(rows),
      runCsv: (csv, o)    => api.runCsv(csv, o?.experiments ?? api.defaultExperiments)(),
      defaultExperiments: api.defaultExperiments,
    };
  };
}
