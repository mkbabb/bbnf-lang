# SK-V8 W0 Hardening V11 - CH5 HIDDEN COUPLING

Date: 2026-05-18.

Target reviewed: `61d5cc3b4312883e026060174e876a0c18b34703`
(`fix(sk-v8-wave0): fold hardening V10 cost and metadata blockers`).

## Verdict

ACCEPT.

Confidence: 96%.

CH5 finds no hidden coupling introduced by V11. The V10 CH4 blockers were folded
inside the W0 report validator without adding a parallel substrate, sidecar
producer, renamed-scanner/Lock 1 path, Track 1 / Track 2 coupling, parser-owned
structural cursor/facts, or telemetry substitution path.

## Evidence

1. Scope stayed report-local. `git show --numstat --format=fuller 61d5cc3b --
   skinny/crates/bbnf-bench/src/report.rs` reports a single touched source file
   with `58 insertions / 109 deletions`. `git diff --name-only HEAD^..HEAD --
   skinny/crates/bbnf-bench/src/report.rs
   skinny/crates/bbnf-bench/src/bin/gate.rs
   skinny/crates/bbnf-bench/src/lock14_baseline.rs
   skinny/crates/bbnf-bench/src/gate.rs` prints only
   `skinny/crates/bbnf-bench/src/report.rs`. This matches W0's telemetry/report
   scope rather than a behavior substrate change.

2. The V10 cost blocker is folded without widening CH5 risk. V10 CH4 rejected
   `3a9fa326` because the live post-V6 footprint was over the `<=120` W0 fold
   cap and empty `arch`, `cpu`, `os`, and `simd` metadata could still pass
   (`restart/skinny/tranches/sk-v8/research/wave-0-hardening/V10/CH4.md:45`,
   `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V10/CH4.md:94`).
   V11's live report.rs footprint from the V8 baseline is under that cap:
   `git diff --numstat 00c3485a..61d5cc3b --
   skinny/crates/bbnf-bench/src/report.rs` reports `118 insertions / 13
   deletions`. The current validator now requires non-empty host `arch`/`cpu`
   and feature `arch`/`os`/`simd` values
   (`skinny/crates/bbnf-bench/src/report.rs:1021`,
   `skinny/crates/bbnf-bench/src/report.rs:1039`,
   `skinny/crates/bbnf-bench/src/report.rs:1053`), with negative tests for the
   formerly accepted empty forms
   (`skinny/crates/bbnf-bench/src/report.rs:2065`,
   `skinny/crates/bbnf-bench/src/report.rs:2068`).

3. No parallel or side substrate is admitted through telemetry. W0 rows still
   require workload-specific substrate tuples:
   `parse_only = borrowed_view_over_offset_tape / discarded_after_capacity /
   one`, `direct_to_struct = sink_only_digest / n/a / zero_or_inert`, and
   `real_typed_struct = typed_direct_projection / n/a / zero_or_inert`
   (`skinny/crates/bbnf-bench/src/report.rs:1063`,
   `skinny/crates/bbnf-bench/src/report.rs:1083`). The report test explicitly
   rejects `substrate_surface = side_substrate`
   (`skinny/crates/bbnf-bench/src/report.rs:2069`).

4. The same-wave consumer is still W0 gate consumption only, which is the
   authorized W0 consumer class. W0 validation rejects any value other than
   `same_wave_consumer_class = gate_only`
   (`skinny/crates/bbnf-bench/src/report.rs:356`), and the gate binary emits the
   same value when building W0 telemetry
   (`skinny/crates/bbnf-bench/src/bin/gate.rs:495`). This does not substitute for
   a W3 production consumer; SPEC keeps W3 production consumption separate and
   blocks telemetry rows as W3 consumers
   (`restart/skinny/tranches/sk-v8/SPEC.md:573`,
   `restart/skinny/tranches/sk-v8/SPEC.md:769`).

5. Sidecar freshness and strict admission remain fail-closed. Strict admission
   rejects non-GO outcomes, deferred rows, view-boundary validation, plane
   mismatch, stale/historical/absent freshness, and any comparator freshness not
   equal to `same-run-native` with `sidecar_freshness = n/a`
   (`skinny/crates/bbnf-bench/src/gate.rs:135`,
   `skinny/crates/bbnf-bench/src/gate.rs:145`,
   `skinny/crates/bbnf-bench/src/gate.rs:157`,
   `skinny/crates/bbnf-bench/src/gate.rs:163`,
   `skinny/crates/bbnf-bench/src/gate.rs:172`). W0 sidecar validation rejects
   `sidecar-same-run` because no structured manifest exists
   (`skinny/crates/bbnf-bench/src/report.rs:1263`,
   `skinny/crates/bbnf-bench/src/report.rs:1287`), and tests cover both strict
   admission and W0 sidecar paths (`skinny/crates/bbnf-bench/src/gate.rs:512`,
   `skinny/crates/bbnf-bench/src/report.rs:1877`).

6. No renamed-scanner, parser-owned cursor/fact, or Track 1 / Track 2 dishonesty
   landed. The Lock 14 baseline marks generated/runtime/tape/SIMD/codegen/Track 2
   behavior roots as frozen (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:375`,
   `skinny/crates/bbnf-bench/src/lock14_baseline.rs:392`), and explicitly forbids
   `UnionTape` in the IR surface (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:462`,
   `skinny/crates/bbnf-bench/src/lock14_baseline.rs:488`). `git diff --name-only
   0bd16f6d..61d5cc3b -- grammars test_data crates/test-fixtures
   crates/runtime/src crates/ir/src crates/passes/src crates/codegen/src
   crates/grammar/src crates/bbnf/src crates/bbnf-simd/src
   crates/bbnf-simd/build.rs crates/bbnf-simd/ext crates/parse-that-regex/src
   crates/bbnf-bench/src/direct_struct.rs
   crates/bbnf-bench/src/real_typed_struct.rs
   crates/bbnf-bench/src/generated_real_typed.rs crates/bbnf-bench/src/track2
   crates/bbnf-bench/src/parity.rs crates/bbnf-bench/src/scan.rs
   crates/bbnf-bench/src/materialization.rs xtask/src/real_typed_schema.rs`
   returned no changed files.

## Verification

- `cargo test -p bbnf-bench w0_ -- --nocapture`: PASS, 12 lib W0 tests and 8
  gate-bin W0 tests.
- `cargo test -p bbnf-bench strict -- --nocapture`: PASS, 5 strict-admission
  tests.
- `cargo test -p bbnf-bench sidecar_same_run -- --nocapture`: PASS, 1 test.
- `cargo test -p bbnf-bench`: PASS, 52 lib tests, 8 gate-bin tests, 0 doctests.
- `CARGO_TARGET_DIR=/tmp/skv8-w0-target RUSTFLAGS='-C target-cpu=native' cargo
  xtask gate-json --advisory --check-results`: PASS.
- `cargo xtask check-json`: PASS.
- `cargo xtask check-real-typed`: PASS.
- `cargo xtask check-conformance`: PASS, 21 valid fixtures accepted and 7
  invalid fixtures rejected.
- `git diff --check`: PASS.

## Blockers

None.

## Required Fold If Rejecting

None. CH5 accepts V11.

## Residual Risk

- This is a W0 CH5 accept only. W0 still needs the required challenge
  convergence before W1-W6 can dispatch.
- W1 still owns replacing `none:pre-W1` CostFacts placeholders with real
  gate-consumed CostFacts before any behavior wave can cite route quality.
- W3 remains blocked on its own accepted plan/challenge; W0 telemetry rows and
  `gate_only` consumption do not authorize structural-projection production work.
