# Agent guidance

## Quality gate

Run before opening a PR (from the repo root):
- `bash scripts/select-env.sh`
- `spago build`
- `spago test`

`scripts/select-env.sh` generates the gitignored `src/Env.purs`; `spago build`
fails with a `PartyCarlo.Env` module-not-found error without it.
