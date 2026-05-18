# SK-V8 W0 Hardening V8 Consolidated

Date: 2026-05-18.

Target: `f452e8373ed717731dd5e720c1d947c086cc22c9`
(`fix(sk-v8-wave0): fold hardening V6 run identity and cost governance`).

## Verdict

REJECT.

Accept rate: 5/6 = 83.3%.

Convergence: not achieved. V8 is not the second qualifying ACCEPT cycle under
`restart/prompts/ORCHESTRATOR.md` Section 3Z because CH1 found a material
strict-admission correctness blocker. The consecutive ACCEPT counter resets.
W1-W6 remain blocked.

## Dispositions

| Lens | Verdict | Confidence | Disposition |
|---|---|---:|---|
| CH1 | REJECT | 94% | A hard-failure row such as `json/canada/parse_only/main` can be converted to a strict measured claim while preserving exact row id, `L / NO-GO`, throughput, and run id; the probe returned `canada_L_strict_DOM: Ok(())`. |
| CH2 | ACCEPT | 94% | Lock 14/generalization checks held in the unchanged re-challenge. |
| CH3 | ACCEPT | 93% | No admitted-row regression, REDRESS reopen, behavior drift, stale/mixed run-id false accept, or schema-only close found. |
| CH4 | ACCEPT | 94% | Cost governance, post-V6 LOC cap, rollback simulation, gate runtime, and behavior freeze held. |
| CH5 | ACCEPT | 95% | Hidden-coupling checks held; local stale Criterion target failed closed rather than admitting rows. |
| CH6 | ACCEPT | 95% | Anti-paper-close checks held; run-id drift and missing Criterion metadata failed closed, and W1-W6 remained blocked pending consolidation. |

## Required V9 Fold

1. W0 validation must freeze current `SK-V8-open` rows as
   `strictness=deferred`, `measured_validation_path=view-boundary`,
   `parse_utf8=view-boundary`, and `escape_complete=yes`; a strict/measured-row
   relabel of any current W0 row must reject.
2. `gate::validate_strict_admission` must reject hard-failure and
   non-admission outcomes before accepting same-run native strict evidence.
   At minimum, `G`, `I`, `J`, `K`, `L`, `M`, `N-direct`, and `S` must not be
   strict-admission eligible.
3. Add focused negatives for the CH1 repro: `canada` parse row with exact
   `L / NO-GO`, exact throughput, exact run id, but strict/measured/DOM
   telemetry must fail W0 validation; helper-level strict admission must reject
   the hard-failure/non-admission outcomes.
4. Preserve all accepted V7/V8 evidence: exact run id, row identity,
   outcome/verdict/throughput baseline, row-manifest Criterion filtering,
   sidecar freshness, Lock 14, frozen behavior-surface diff, cost accounting,
   and rollback protocol.

## Evidence To Rerun After Fold

- `cargo test -p bbnf-bench w0_ -- --nocapture`
- `cargo test -p bbnf-bench strict -- --nocapture`
- `cargo test -p bbnf-bench sidecar_same_run -- --nocapture`
- `cargo test -p bbnf-bench`
- `CARGO_TARGET_DIR=/tmp/skv8-w0-target RUSTFLAGS='-C target-cpu=native' cargo xtask gate-json --advisory --check-results`
- dynamic Criterion mutation probes for future rows, valid-fixture/unvalidated
  rows, and run-id drift
- `cargo xtask check-json`
- `cargo xtask check-real-typed`
- `cargo xtask check-conformance`
- frozen behavior-surface diff
- `git diff --check`

## Governance

V8 rejection resets the consecutive ACCEPT counter. After the V9 fold, W0 must
again receive two consecutive challenge cycles at >=95% ACCEPT, with zero
critical defects and no unresolved REVISE, before W0 can close and W1-W6 can
dispatch.
