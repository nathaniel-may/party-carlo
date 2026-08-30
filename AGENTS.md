# Agent guidance

Comments carry the non-obvious why — the constraint that forced this shape, the simpler thing that does not work. Never narrate the line below. A comment that disagrees with its code is worse than none, so when the code changes, trim the claim to match.

A test earns its place by being able to fail. One that asserts against a copy of the logic, or against the language itself, gets deleted rather than annotated. Accepted-gap notes belong in the PR description, not in the code.

In committed files, only reference paths that exist in a fresh clone. Scratch files from a local agent run (design docs, plans, review notes) live outside the repo and must never be cited in code, comments, tests, docs, or PR descriptions.

Write an ADR when a choice constrains what the tool computes or claims — not how it is built — and would be plausibly reverted by someone who cannot see why it was made. Tradeoffs and appealing but rejected alternatives should be documented in the PR description instead. Obviously bad choice alternatives need no mention.

Error level logs break tests by design via `failOnLog` in `test/Unit.purs`. Error level logs indicate something in the code is broken and needs to be fixed. For example, users using malformed input is expected control flow not a bug to fix and can use info or debug instead.

`Display` is for human-facing prose and uses techniques like appending units and adding visual separators. Use `show` for anything machine-read like CSV cell data.

## Quality gate

Run before opening a PR (from the repo root):
- `npm install`
- `bash scripts/select-env.sh`
- `spago build`
- `spago test`

`scripts/select-env.sh` generates the gitignored `src/Env.purs`; `spago build`
fails with a `PartyCarlo.Env` module-not-found error without it.
