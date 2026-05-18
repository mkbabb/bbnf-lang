# SK-V8 W0 Hardening V10 CH5

Date: 2026-05-18.

Target reviewed: `3a9fa32675cedb1f8a0d31247df229fe505068be`
(`fix(sk-v8-wave0): fold hardening V9 telemetry consumption blocker`).

## Verdict

ACCEPT.

Confidence: 96%.

CH5 finds that V10 can count as the first qualifying ACCEPT cycle after the V9
rejection, subject to the other V10 challenge lanes also returning qualifying
ACCEPT results. This does not close W0 by itself: V9 rejection preserved the
reset counter, so W0 still needs one more consecutive qualifying ACCEPT cycle
after V10 before W1-W6 can dispatch.

## Evidence

V9 rejected because required W0 telemetry was rendered as gate-consumed while
several fields were only checked for non-empty text. The V10 fold moves those
fields under `validate_w0_manifest_semantics()` and makes them executable gate
inputs: exact pre-W1 CostFacts sentinel, `redress_entry=none`,
`track2_independence_status=independent_verified`, structured build metadata,
and workload-specific substrate tuple validation
(`skinny/crates/bbnf-bench/src/report.rs:1007`,
`skinny/crates/bbnf-bench/src/report.rs:1031`,
`skinny/crates/bbnf-bench/src/report.rs:1091`). That closes the V9
producer-only telemetry blocker for the CH5 no-paper-close lens.

The hidden-coupling/substrate substitution path is now falsifiable by workload:
`parse_only` must report
`borrowed_view_over_offset_tape / discarded_after_capacity / one`,
`direct_to_struct` must report `sink_only_digest / n/a / zero_or_inert`, and
`real_typed_struct` must report
`typed_direct_projection / n/a / zero_or_inert`
(`skinny/crates/bbnf-bench/src/report.rs:1091`,
`skinny/crates/bbnf-bench/src/report.rs:1118`). A side substrate or inert
structural projection relabel no longer survives while preserving row id,
throughput, outcome, and comparator evidence.

The strict-vs-strict discipline from the V8 fold remains intact. Helper-level
strict admission rejects any non-`GO` outcome before comparator evidence
(`skinny/crates/bbnf-bench/src/gate.rs:135`,
`skinny/crates/bbnf-bench/src/gate.rs:144`), and W0 report validation still
freezes current rows as `strictness=deferred`,
`measured_validation_path=view-boundary`, `parse_utf8=view-boundary`, and
`escape_complete=yes` (`skinny/crates/bbnf-bench/src/report.rs:1121`,
`skinny/crates/bbnf-bench/src/report.rs:1146`). Native comparator evidence is
still exact by workload plane, strictness, same-run-native freshness, source
artifact, and present Mbps (`skinny/crates/bbnf-bench/src/report.rs:1338`,
`skinny/crates/bbnf-bench/src/report.rs:1399`); sidecar same-run claims still
reject without a structured manifest (`skinny/crates/bbnf-bench/src/report.rs:1312`).

The V10 tests mutate the formerly under-consumed fields while preserving the
accepted W0 row shape and assert rejection for CostFacts, CostFacts
alternatives, redress, Track 2 independence, build flags, host metadata,
feature mask, and substrate surface
(`skinny/crates/bbnf-bench/src/report.rs:2087`,
`skinny/crates/bbnf-bench/src/report.rs:2120`). This is enough to make the
gate challengeable rather than a prose promise.

No behavior-surface drift was found. The V10 implementation fold changes only
`skinny/crates/bbnf-bench/src/report.rs`; the frozen diff from `0bd16f6d..HEAD`
over JSON grammar, runtime JSON/tape, SIMD, codegen, generated/product helper,
Track 2, parity, scan, and materialization paths is empty.

## Verification

- `cargo test -p bbnf-bench w0_ -- --nocapture`: PASS, 12 W0 lib tests and 8
  gate-bin W0 tests.
- `cargo test -p bbnf-bench strict -- --nocapture`: PASS, 5 strict-admission
  tests.
- `cargo test -p bbnf-bench sidecar_same_run -- --nocapture`: PASS.
- `cargo test -p bbnf-bench`: PASS, 52 lib tests, 8 gate-bin tests, 0 doctests.
- `CARGO_TARGET_DIR=/tmp/skv8-w0-target RUSTFLAGS='-C target-cpu=native' cargo xtask gate-json --advisory --check-results`: PASS.
- `cargo xtask check-json`: PASS.
- `cargo xtask check-real-typed`: PASS.
- `cargo xtask check-conformance`: PASS, 21 valid fixtures accepted and 7
  invalid fixtures rejected.
- Frozen behavior-surface diff: PASS, no changed files.
- `git diff --check`: PASS.

## Blockers

None.

## Required Fold

None for CH5.

## Residual Risks

- V10 is only the first possible qualifying ACCEPT cycle after V9. W0 remains
  blocked until a second consecutive qualifying ACCEPT cycle lands and is
  consolidated.
- W1 still owns replacing `none:pre-W1` CostFacts placeholders with real
  gate-consumed CostFacts evidence before behavior waves can cite route
  quality.
