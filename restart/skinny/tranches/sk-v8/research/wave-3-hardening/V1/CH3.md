# SK-V8 W3 Hardening V1 CH3

Reviewed target: `fc91c217`
(`docs(sk-v8-wave3-plan): reject Tier A implementation on fit gate`).

Verdict: ACCEPT

Confidence: 94%

## Findings

1. The reject-before-implementation disposition is coherent. SPEC Section 6
   requires W3 to return REVISE before dispatch when the exact fit estimate
   exceeds the W3 LOC budget or the 90-minute implementation/redress cap, and
   the plan does that before authorizing source edits. The scanner/tape
   mismatch is concrete: scanner positions are structural punctuation plus real
   quotes, while retained tape offsets are generated parser events including
   container opens/closes, opening quotes, number starts, and literal starts.

2. Row-gate accounting is present. The plan selects the two structural-heavy
   parse rows `twitter/parse_only` and `apache_builds/parse_only`, names guard
   rows `canada/parse_only`, `mesh/parse_only`, `numbers/parse_only`, and
   `marine_ik/parse_only`, and keeps the full-table maintain rule at no worse
   than -2.0% Track 1 and Track 2 versus `SK-V8-open`.

3. Fit-gate accounting is sufficient for rejection. The owner-path list spans
   SIMD, scanner, retained tape, generated parser/view/value, matching codegen
   templates, generated JSON output, bench parity/materialization/gate code,
   `RESULTS.md`, and `REDRESS.md`. That is a representation redesign across
   more surfaces than a bounded W3 patch, so the plan's conclusion that it
   exceeds the 450 LOC default budget, the exceptional 650 LOC budget, and the
   90-minute redress cap is defensible.

4. Same-wave consumer accounting fails closed. The plan rejects telemetry
   substitutes (`tape_vs_tape`, `simd_structural_scan`, Track 2, comparator
   rows, and retained-view-only checks) and requires any future W3-like slice to
   make generated JSON retained Track 1 parsing consume retained tape
   positions/classes in the measured row, with retained view/`ValueRef` parity
   proven in the same slice.

5. Scalar/checkasm requirements are not bypassed. No primitive wiring is
   authorized by this plan, and REDRESS 88/89 PMULL and CTZ/bulk bodies remain
   pre-blocked. The SPEC scalar/checkasm gate therefore remains not applicable
   to this rejection slice, but would still apply before any future primitive
   implementation.

6. Generated-output audit and revert-slice accounting are adequate for a
   reject plan. The plan names generated JSON output under
   `skinny/crates/runtime/src/grammars/json/`, includes generated output, row
   measurement, Lock 14 proof, full-table maintain, and rollback in the
   unbounded-cost rationale, and routes follow-up to REDRESS plus HANDOFF rather
   than silently landing a source patch.

## Verification

- `git rev-parse HEAD`: `fc91c2173e8451dd06733381346bd800b0711f6e`.
- `git show --stat --oneline --decorate --no-renames fc91c217`: plan commit
  only; `skv8-W3-plan.md` added with 114 insertions.
- `cargo test -p bbnf-bench offset_stream_tracks_verified_source_events -- --nocapture`
  from `skinny/`: PASS.
- `cargo test -p bbnf-bench counts_json_lazy_tape_materialization_shape -- --nocapture`
  from `skinny/`: PASS.
- `git diff --exit-code HEAD -- skinny/RESULTS.md`: PASS.
- `git diff --check`: PASS.

## Required Folds

None for accepting the reject-before-implementation plan. The accepted plan's
own redress sequence remains: add the REDRESS entry for the scanner/tape
event-model mismatch, update HANDOFF to mark W3 rejected/routed and unblock W4,
and feed the split event-grammar/`ValueRef` precursor to SK-V9/Pass Omega.
