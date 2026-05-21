# SK-V12 W1b-2b CHALLENGE CH4: Cost / LOC / Test Budget

Date: 2026-05-20.
Lens: CH4 cost.
Scope: W1b-2b Section 7.2 CSS L4 lightningcss SOTA report/gate.
Output: this file.

## Verdict

REVISE.

The plan is pointed at the right Section 7.2 problem, but it is too broad for
its declared `<=220` hand/test LOC and `<=30 min` redress cap. The requested
slice is not just a companion flag: it adds a new 50-plus-field report schema,
strict validation, derived threshold/margin checks, Criterion `new/` artifact
consumption, same-run artifact freshness, CLI no-write/mixed-flag behavior,
JSON guard continuation, an emitted report artifact, REDRESS/RESULTS routing,
and a sizable test matrix. That is a plausible wave only if the plan either
raises the budget or cuts the implementation surface.

## Evidence

- SPEC Section 7.2 authorizes W1b-2b as high risk with `<=220 report/gate/test`
  and `<=30 min`, but its tasks include schema validation, a new gate flag,
  Criterion estimate consumption, JSON guard proof, and fail-closed write/probe
  handling.
- The current owner files are already large: `report.rs` is 3325 LOC and
  `gate.rs` is 2554 LOC. Existing companion-report support helps the CLI side,
  but a distinct `sk-v12-css-l4-sota-v1` struct and validator still need many
  new fields and negative checks.
- A2 keeps the CLI extension narrow, but also requires duplicate/mixed report
  rejection, stale RESULTS guidance, JSON guard continuation, and CSS-only
  guard-root rejection. That is more than a flag parser patch if fully tested.
- A3 requires actual Criterion `new/benchmark.json`, `new/estimates.json`, and
  `new/sample.json` consumption for three lanes, derived Mbps, sample-count
  checks, no `base/` fallback, stale-artifact rejection, and equality proof
  binding. The current plan does not isolate this into a small helper budget.
- A6 asks for at least five report tests, five CLI tests, an integration-style
  no-write command check, a negative empty-root command check, and the minimum
  command set including `nonjson_css_l4`, two new test filters, and `lock14`.
  That compile/test loop alone can consume the 30-minute redress cap.

## Required Revision

Before redress, choose one of these narrower shapes:

1. Keep the `<=220` cap by making W1b-2b a report/gate validator only: consume
   precomputed report fields, derive threshold and margin, validate artifact
   path names and JSON guard state, add focused unit tests, and defer live
   Criterion file parsing to the report writer or a later wave.
2. Keep live Criterion parsing and no-write integration checks, but raise the
   source/test budget and wall-clock expectation. That route should name a
   hard sub-budget for schema structs, validation, Criterion helpers, CLI, unit
   tests, and command checks.

The revised plan must also clarify whether `skinny/RESULTS.md` and
`skinny/REDRESS.md` are implementation-owned in W1b-2b or outcome-only files.
For `PASS-MEASURED-BASELINE`, Section 7.2 says `RESULTS.md` must not move, so
the default redress path should not budget source or command time for result
rewrites.

## Redress Boundary

Do not authorize the current plan as written under `<=220` LOC. Authorize after
revision if the implementation slice is narrowed to the existing companion
helper pattern, the report validator has a counted field/test budget, Criterion
parsing is either explicitly deferred or separately budgeted, and the required
command set is reduced to what can realistically run inside the 30-minute cap.

DISPOSITION: REVISE.
