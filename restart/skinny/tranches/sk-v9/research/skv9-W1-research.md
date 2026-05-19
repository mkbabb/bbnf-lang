# SK-V9 W1: Apache/CITM Typed Row Admission Research

Date: 2026-05-18.
Scope: Archive the P2-C evidence for W1 Apache/CITM measured typed-row
admission.
Output: this file.

## §1 — Findings

1. W1 is dispatchable. `SPEC.md` Section 4 says W0 is closed,
   `G-S-P1-RERUN-CONVERGED` is PASS, and W1 is an independent
   row-table admission wave for Apache/CITM typed rows. The objective is
   to promote `apache_builds/real_typed_struct` and
   `citm_catalog/real_typed_struct` from source/product parity into
   measured `A / GO` rows.
2. The source/product proof already exists. `skv9-p2-C-apache-citm-admission.md`
   §1 ties the gap to REDRESS 91: Apache/CITM typed parsers and parity
   were admitted, but the measured row table was not expanded.
3. The owner surface is bounded to row-table and gate metadata work:
   `skinny/crates/bbnf-bench/src/report.rs`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs`, `skinny/RESULTS.md`,
   the fresh Criterion capture directory, `skinny/REDRESS.md`,
   `restart/skinny/tranches/sk-v9/HANDOFF.md`, and
   `restart/locks/LOCKS.md`.
4. Current gate code confirms the admission gap:
   `w0_real_typed_metadata_expected("apache_builds")` and
   `w0_real_typed_metadata_expected("citm_catalog")` are still false
   because the measured baseline table lacks those real typed row ids.
5. Existing W0 rows already contain the comparator shape W1 must use:
   `sonic_rs_strict` and `serde_json` are same-run native typed-direct
   comparators for current real typed rows, while C++ sidecars are
   explicitly absent for `real_typed_struct`.

## §2 — Recommendations

1. Use the co-promotion path from P2-C §2.3: keep the `SK-V9-open`
   wave id, produce a fresh Criterion fingerprint, render a uniform
   `sk-v9-open:criterion-fnv64-<16 hex>` run id across all RESULTS rows,
   and add only the two Apache/CITM `real_typed_struct` baseline entries.
2. Flip only the two W1 assertions in
   `w0_real_typed_metadata_expectation_uses_measured_baseline_not_source_fixtures`.
   The test remains the consumer for the baseline-driven metadata rule.
3. Admit each row only if Track 1 real typed Mbps clears
   `ceil(sonic_rs_real_typed_struct / 1.10)` and full-fixture
   `assert_real_typed_parity` remains green.
4. Keep the four existing typed GO rows as guards: twitter,
   update_center, mesh, and marine_ik must remain `A / GO`.
5. Run the W1 verification matrix from P2-C §2.9, with
   `cargo xtask gate-json --advisory --check-results` as the final
   RESULTS consumer.

## §3 — Risks

1. REDRESS 91 stays binding for `canada/real_typed_struct`; W1 admits
   Apache/CITM only.
2. REDRESS 92 is not reopened because W1 does not touch retained parse,
   structural substrate, or cursor proof surfaces.
3. REDRESS 93 is not reopened because W1 does not alter direct
   admission or guard-plane behavior.
4. Lock 14 needs a scoped W1 allowance because the W1 owner surface
   reaches report/gate/results/status documents outside the old
   `sk-v8-real-typed-w2` source-product slice.

## §4 — Sources

1. `restart/skinny/tranches/sk-v9/SPEC.md` Section 4.
2. `restart/skinny/tranches/sk-v9/research/p2/skv9-p2-C-apache-citm-admission.md`.
3. `skinny/REDRESS.md` Item 91.
4. `skinny/RESULTS.md` SK-V9-open manifest.
5. `skinny/crates/bbnf-bench/src/report.rs`.
6. `skinny/crates/bbnf-bench/src/bin/gate.rs`.
