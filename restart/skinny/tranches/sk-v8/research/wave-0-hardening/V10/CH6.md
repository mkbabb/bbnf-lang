# SK-V8 W0 Hardening V10 CH6

Date: 2026-05-18.

Target: `3a9fa32675cedb1f8a0d31247df229fe505068be`
(`fix(sk-v8-wave0): fold hardening V9 telemetry consumption blocker`).

## Verdict

ACCEPT.

Confidence: 95%.

This is an integration/CI-style accept for the V10 fold. It is not W0 closure.
V9 consolidated as REJECT because required W0 manifest fields were still
under-consumed, and the reset consecutive-ACCEPT counter remains governed by
the V9 consolidated note. If V10 consolidates cleanly, it can be the first
qualifying cycle after that reset; W0 still needs a second consecutive
qualifying ACCEPT cycle with zero critical defects and no unresolved REVISE
before W0 closes or W1-W6 dispatch.

## Reviewed Surfaces

- V9 rejection and required V10 fold:
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V9/HARDENING-W0-V9-CONSOLIDATED.md:29`.
- W0 telemetry and same-wave consumer contract:
  `restart/skinny/tranches/sk-v8/SPEC.md:103`,
  `restart/skinny/tranches/sk-v8/SPEC.md:142`,
  `restart/skinny/tranches/sk-v8/SPEC.md:346`,
  `restart/skinny/tranches/sk-v8/SPEC.md:360`.
- W0 measured state and downstream block:
  `restart/skinny/tranches/sk-v8/HANDOFF.md:31`,
  `restart/skinny/tranches/sk-v8/HANDOFF.md:127`,
  `restart/skinny/tranches/sk-v8/HANDOFF.md:180`,
  `restart/skinny/tranches/sk-v8/HANDOFF.md:236`.
- V10 source fold:
  `skinny/crates/bbnf-bench/src/report.rs:355`,
  `skinny/crates/bbnf-bench/src/report.rs:1007`,
  `skinny/crates/bbnf-bench/src/report.rs:1036`,
  `skinny/crates/bbnf-bench/src/report.rs:1091`,
  `skinny/crates/bbnf-bench/src/report.rs:2087`.
- Gate producer and committed evidence:
  `skinny/crates/bbnf-bench/src/bin/gate.rs:319`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:472`,
  `skinny/RESULTS.md:44`,
  `skinny/RESULTS.md:138`.

## Evidence

- `git rev-parse HEAD`: `3a9fa32675cedb1f8a0d31247df229fe505068be`.
- `git show --stat --oneline 00c3485a..3a9fa326`: V10 implementation changes
  only `skinny/crates/bbnf-bench/src/report.rs` with 169 insertions and 13
  deletions; V9 hardening docs are archival additions.
- `git diff --check 00c3485a..3a9fa326 --`: PASS.
- `awk` row count over `skinny/RESULTS.md`: `main_rows=38`,
  `manifest_rows=38`.
- Frozen behavior-surface diff from `0bd16f6d..HEAD` over runtime, SIMD,
  codegen, generated/product helpers, Track 2, parity, scan, and materialization
  returned no paths.
- Both W0 plan frozen-path `git diff --exit-code -- ...` commands returned
  clean.

Commands run from `/Users/mkbabb/Programming/bbnf-lang/skinny`:

- `CARGO_TARGET_DIR=/tmp/skv8-ch6-v10-target cargo test -p bbnf-bench w0_ -- --nocapture`:
  PASS; 12 report W0 tests and 8 gate-bin W0 tests passed.
- `CARGO_TARGET_DIR=/tmp/skv8-ch6-v10-target cargo test -p bbnf-bench strict -- --nocapture`:
  PASS; 5 strict-admission tests passed.
- `CARGO_TARGET_DIR=/tmp/skv8-ch6-v10-target cargo test -p bbnf-bench sidecar_same_run -- --nocapture`:
  PASS; sidecar same-run without structured manifest rejected.
- `CARGO_TARGET_DIR=/tmp/skv8-ch6-v10-target cargo test -p bbnf-bench`:
  PASS; 52 library tests, 8 gate-bin tests, and doc tests passed.
- `CARGO_TARGET_DIR=/tmp/skv8-w0-target RUSTFLAGS='-C target-cpu=native' cargo xtask gate-json --advisory --check-results`:
  PASS against committed `skinny/RESULTS.md`; output retained overall
  `N-direct / NoGo`.
- `CARGO_TARGET_DIR=/tmp/skv8-ch6-v10-target cargo xtask check-json`: PASS.
- `CARGO_TARGET_DIR=/tmp/skv8-ch6-v10-target cargo xtask check-real-typed`:
  PASS.
- `CARGO_TARGET_DIR=/tmp/skv8-ch6-v10-target cargo xtask check-conformance`:
  PASS; `21 valid fixtures accepted; 7 invalid fixtures rejected`.

## Findings

1. No blocker: the V9 telemetry-consumption blocker is folded. V10 replaces the
   previous presence-only gap with `validate_w0_manifest_semantics()`, which
   enforces exact pre-W1 CostFacts sentinels, `redress_entry=none`,
   `track2_independence_status=independent_verified`, structured build/host/
   feature metadata, and workload-specific substrate tuples before W0 admission.

2. No blocker: focused negatives cover the V10-required field groups while
   preserving row identity, outcome/verdict, throughput, run id, and comparator
   evidence. The exact-baseline test now mutates CostFacts, rejected
   alternatives, redress, Track 2 status, build flags, host facts, feature mask,
   and substrate surface and requires `validate_sk_v8_w0()` failure.

3. No blocker: source, RESULTS, SPEC, and HANDOFF are consistent for CH6. The
   gate path renders and validates the committed `RESULTS.md`; the report has
   38 current main rows and 38 W0 manifest rows; SPEC/HANDOFF still state W0
   telemetry-only authority, the same row family counts, `N-direct / NoGo`, and
   W1-W6 blocked behind W0 closure plus later wave entry gates.

4. No blocker: the fold remains telemetry/report-gate scoped. The frozen
   parser/runtime/SIMD/codegen/product/generated surfaces have no diff, and the
   required W0 local reproduction commands are green from the skinny workspace.

## Blockers

None found for CH6.

## Required Fold If Rejecting

Not applicable; CH6 accepts V10. Consolidation should preserve the exact
skinny-workspace reproduction commands and must not describe V10 as W0 closure.
