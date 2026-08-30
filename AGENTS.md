# Agent guidance

Comments carry the non-obvious why — the constraint that forced this shape, the simpler thing that does not work. Never narrate the line below. A comment that disagrees with its code is worse than none, so when the code changes, trim the claim to match.

A test earns its place by being able to fail. One that asserts against a copy of the logic, or against the language itself, gets deleted rather than annotated: an accepted-gap note is a note nobody acts on.

Nothing in this repo may cite a file that is not reachable. Documents constructed for a local workflow are not included in the repo nor on the web and should not be referenced.

ADRs should be made for choices that affect the main value prop of the code, and are at risk of being reverted by a well-meaning future change.

Error level logs break tests by design. Error level logs indicate something in the code is broken and needs to be fixed. For example, users using malformed input is expected control flow not a bug to fix and can use info or debug instead.

`Display` is for human-facing prose and uses techniques like appending units and adding visual separators. Use `show` for anything machine-read like CSV cell data.

## Quality gate

Run before opening a PR (from the repo root):
- `bash scripts/select-env.sh`
- `spago build`
- `spago test`

`scripts/select-env.sh` generates the gitignored `src/Env.purs`; `spago build`
fails with a `PartyCarlo.Env` module-not-found error without it.
