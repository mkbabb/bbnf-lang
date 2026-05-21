# SK-V12 W1b-2b CH4 - Cost / Budget

Date: 2026-05-20.
Wave: W1b-2b - CSS L4 Lightningcss SOTA Report + Admission Gate.
Lens: CH4 cost / budget.
Disposition: REVISE.

## Blocking Findings

PLAN-V2 is directionally narrowed, but the stated `<=220 report/gate/test LOC`
and 30-minute redress are not credible for the executable checks it requires.
The dedicated `sk-v12-css-l4-sota-v1` schema is not a small wrapper around the
existing `SkV12NonJsonReport`: it adds lightningcss-specific fields,
three-way equality state, admission status, threshold/margin derivation, and
new exact-value invariants. The row struct alone is roughly 40 fields; with
`deny_unknown_fields`, report identity, one-row validation, fixture checks,
threshold/margin tolerance, and focused tests, `report.rs` is realistically
135-170 LOC, not `<=90`.

The `gate.rs` work is also under-budgeted. Reusing the companion parser is
cheap, but PLAN-V2 additionally requires a CSS companion branch, mixed-flag
rejection updates, three-lane Criterion reads, `benchmark.json` byte checks,
`estimates.json` mean reads, `sample.json` sample-count checks, Mbps
recomputation, report-value comparison, and CLI tests. That is realistically
90-125 LOC, not the planned `<=80`.

The redress command shape hides extra scope. PLAN-V2 says the CSS gate reads
CSS lanes from `criterion_root()/nonjson_css_l4/`, then the same invocation
sets `CRITERION_HOME=/tmp/skv12-w1a-json-guard-criterion` for JSON guards. The
current `gate.rs` has one `criterion_root()`, so the CSS gate would look for
`nonjson_css_l4` inside the JSON guard root. Fixing that inside this wave
requires either a new CSS Criterion root flag, report-path-root semantics, or a
two-command evidence protocol. That is not accounted for in the LOC or
30-minute budget.

`skinny/RESULTS.md` remains an unbounded cost branch. Current W1b-2a numbers
make `PASS-ADMIT-CANDIDATE` likely, but adding a CSS row to RESULTS while
preserving the existing JSON stale-results check can require renderer or gate
stale-check changes beyond the named report/gate validator. PLAN-V2 should not
leave this as an optional redress-time decision.

## Required Revisions

Revise PLAN-V2 before redress by making one of these budget shapes explicit:

- Preferred: W1b-2b is a measured companion-gate admit candidate only. It does
  not move `skinny/RESULTS.md` even on `PASS-ADMIT-CANDIDATE`; final RESULTS
  reconciliation is routed to W5 close. The evidence protocol uses two
  commands: CSS gate against the CSS Criterion root, then JSON guard/stale
  check against `/tmp/skv12-w1a-json-guard-criterion`.
- Alternative: keep same-wave RESULTS movement, but raise the source budget to
  cover the renderer/stale-check delta and name the exact marker or render
  contract that lets JSON results remain gate-checkable.

Also revise the implementation estimate to either split source and measurement
into separate sub-waves, or raise the single-wave budget to roughly 300
report/gate/test LOC. If the 30-minute redress cap is immovable, split the
work; a single source+artifact+bench+JSON-guard+REDRESS pass is too tight for
this first CSS SOTA admission gate.

## Verdict

REVISE. The plan is conceptually bounded, but it still underprices the schema
and Criterion verifier and leaves the CSS-root/JSON-root and RESULTS movement
branches unresolved.
