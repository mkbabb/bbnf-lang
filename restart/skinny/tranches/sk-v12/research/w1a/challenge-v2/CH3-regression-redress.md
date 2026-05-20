# SK-V12 W1a CHALLENGE V2 - CH3 Regression / REDRESS

Date: 2026-05-20.
Lens: CH3 regression / REDRESS.
Disposition: REVISE.

## Finding

`PLAN-V2.md` fixes the main V1 CH3 guard gaps: the selected JSON-touching route
now requires `json_guard_state = refreshed:<run-id>:guards-pass`;
`not_refreshed:no_behavior_drift` is explicitly invalid for this route. The
exact SK-V12 floor verifier is also correctly additive to `gate-json`:
`gate-json --check-results` proves `skinny/RESULTS.md` is the exact rendered
report, then `verify-skv12-json-floors.awk` enforces the SPEC Section 0.5
direct and typed Track 1 / Track 2 floors. Current `skinny/RESULTS.md` passes
that verifier.

The remaining CH3 blocker is rejected-patch coverage. `PLAN-V2.md` declares
`scan.rs` and `sink.rs` JSON-owned source files and says their generated
headers are removed or replaced, but the owner roster and
`/tmp/skv12-waveW1a-rejected.patch` command omit:

- `skinny/crates/runtime/src/grammars/json/scan.rs`
- `skinny/crates/runtime/src/grammars/json/sink.rs`

If those files move during redress, the rejected patch would not capture the
full W1a slice and the revert/accounting record would be incomplete.

## Required Revision

Before redress dispatch, revise `PLAN-V2.md` to either:

1. add `scan.rs` and `sink.rs` to the editable source roster and rejected-patch
   command, or
2. state that W1a V2 does not edit those files and defers header/comment
   cleanup.

Keep the existing generated-output coverage, including `generated_real_typed.rs`
when typed regen changes.

## REDRESS 121 Requirements

REDRESS 121 must record W1a only as `G-W1a-GRAMMARCONFIG-LOCK14`, not CSS
admission or SK-V12 close. Evidence must include refreshed JSON guard run id,
`gate-json --check-results`, `gate-json --with-cost-facts --check-results`, the
exact AWK floor verifier output, generated-size facts, and no CSS/non-JSON row
movement.

After the rejected-patch slice is fixed, CH3 has no remaining objection.
