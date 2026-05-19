# SK-V9 Wave W1 Plan: Apache/CITM Measured Typed-Row Admission

Inputs: `restart/skinny/tranches/sk-v9/SPEC.md` Section 4;
`restart/skinny/tranches/sk-v9/research/skv9-W1-research.md`;
`restart/skinny/tranches/sk-v9/research/p2/skv9-p2-C-apache-citm-admission.md`;
`skinny/RESULTS.md`; `skinny/REDRESS.md` Item 91.

Intervention: promote `apache_builds/real_typed_struct` and
`citm_catalog/real_typed_struct` to measured `A / GO` rows by adding
only their row-table baseline entries, flipping the measured-baseline
metadata expectations, rendering a fresh same-run RESULTS manifest, and
recording the admission in REDRESS.

Owner paths:

- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/RESULTS.md`
- `skinny/crates/bbnf-bench/target/skv9-w1/criterion/`
- `skinny/REDRESS.md`
- `restart/skinny/tranches/sk-v9/HANDOFF.md`
- `restart/locks/LOCKS.md`

Falsifiability gate: `G-W1-TYPED-ADMISSION` from `SPEC.md` Section 4.
The redress phase passes only if:

- a fresh Criterion capture is produced with
  `RUSTFLAGS="-C target-cpu=native"` and a uniform
  `sk-v9-open:criterion-fnv64-<16 hex>` run id;
- `apache_builds/real_typed_struct` Track 1 Mbps is at least
  `ceil(sonic_rs_real_typed_struct / 1.10)`;
- `citm_catalog/real_typed_struct` Track 1 Mbps is at least
  `ceil(sonic_rs_real_typed_struct / 1.10)`;
- twitter, update_center, mesh, and marine_ik `real_typed_struct` rows
  remain `A / GO`;
- Apache remains `direct_to_struct N-direct / NO-GO` and CITM remains
  `direct_to_struct A / GO`;
- `assert_real_typed_parity` passes for the promoted rows;
- `cargo xtask gate-json --advisory --check-results` succeeds after
  `RESULTS.md` is rendered;
- `cargo test -p bbnf-bench lock14_baseline -- --nocapture` succeeds.

Hard cap: 75 minutes redress, inside the W1 `<=90 min` wave cap.

Revert protocol: if metadata expectation or Lock 14 fails, revert the
two baseline entries, the two assertion flips, and the Lock 14
allowance; if RESULTS rendering or `gate-json --check-results` fails,
restore the pre-W1 `RESULTS.md`; if either promoted row misses the
typed Track 1 threshold, save the failed diff to
`/tmp/skv9-waveW1-rejected.patch`, record a measured REDRESS rejection,
and do not leave the row-table promotion in tree.

Same-wave consumer: the report/gate path is the consumer for this
row-table-only wave. `SK_V8_OPEN_BASELINE` drives
`w0_real_typed_metadata_expected`, `required_metadata_specs`, the
rendered telemetry rows, and `gate-json --check-results` in the same
commit. No parser, runtime, SIMD, or codegen behavior is introduced.

Pre-blocked routes:

- REDRESS 91: W1 clears only the deferred Apache/CITM measured-row
  whitelist gap. `canada/real_typed_struct` remains rejected.
- REDRESS 92: no retained class/event grammar, structural substrate, or
  cursor proof surface is touched.
- REDRESS 93: direct guard-plane rows are not modified.
- REDRESS 60-72 and 85-87: no retained-parse sidecar, JSON policy leak,
  parser-owned scratch, or generic-crate behavior change is introduced.

Redress execution checklist:

1. Preflight: run `cargo test -p bbnf-bench real_typed -- --nocapture`
   and `cargo xtask check-real-typed`.
2. Capture: run `RUSTFLAGS="-C target-cpu=native" CRITERION_HOME=target/skv9-w1/criterion cargo xtask bench-json`
   unless an equivalent fresh capture already exists for the exact HEAD.
3. Promote: add Apache/CITM baseline entries, flip the two gate
   assertions, and render `RESULTS.md` with
   `CRITERION_HOME=target/skv9-w1/criterion cargo xtask gate-json --advisory --update-results`.
4. Record: add the W1 REDRESS entry, update HANDOFF status, and add the
   scoped Lock 14 allowance.
5. Verify: run `cargo test -p bbnf-bench lock14_baseline -- --nocapture`,
   `cargo test -p bbnf-bench real_typed -- --nocapture`,
   `cargo xtask check-real-typed`, `cargo xtask check-json`,
   `cargo xtask check-conformance`,
   `cargo xtask gate-json --advisory --check-results`, and
   `git diff --check`.
