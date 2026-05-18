# SK-V8 W2 Hardening V4 - CH4

Date: 2026-05-18.
Reviewer: CH4.
Target reviewed: `74fe4e1b`
(`fix(sk-v8-wave2-gate): bind real typed metadata expectations to measured W0 rows`).

## Verdict

ACCEPT.

Confidence: 94%.

## Findings

1. The V3 checked-report blocker is folded without admitting Apache/CITM
   benchmark rows. `validate_w0_capture_metadata` now receives
   `w0_real_typed_metadata_expected(&fixture.name)` instead of deriving the
   requirement from the broader source fixture map
   (`skinny/crates/bbnf-bench/src/bin/gate.rs:57-63`). That predicate keys off
   `sk_v8_open_baseline("json/{fixture}/real_typed_struct/main")`, so required
   real typed metadata follows the W0 measured row table
   (`skinny/crates/bbnf-bench/src/bin/gate.rs:1115-1117`;
   `skinny/crates/bbnf-bench/src/report.rs:931-935`).

2. The measured baseline surface remains exactly the W0 typed rows. The
   baseline table contains `twitter`, `update_center`, `mesh`, and `marine_ik`
   `real_typed_struct` rows
   (`skinny/crates/bbnf-bench/src/report.rs:678-684`;
   `skinny/crates/bbnf-bench/src/report.rs:755-761`;
   `skinny/crates/bbnf-bench/src/report.rs:770-776`;
   `skinny/crates/bbnf-bench/src/report.rs:813-819`). `skinny/RESULTS.md`
   matches that shape and has no measured `apache_builds/real_typed_struct` or
   `citm_catalog/real_typed_struct` rows.

3. The regression test covers the source-only split that V3 required. It
   asserts `twitter` and `update_center` require real typed metadata while
   `apache_builds` and `citm_catalog` do not
   (`skinny/crates/bbnf-bench/src/bin/gate.rs:1718-1724`). The checked gate can
   still reject extra or incoherent local Criterion rows, but source/product
   fixture presence alone no longer forces unadmitted metadata.

4. Lock 14 and frozen-surface boundaries hold. The commit changes the report
   gate, CH reports, HANDOFF, and REDRESS, but no frozen parser/runtime/
   substrate/direct/product/generated paths and no `skinny/RESULTS.md` rows.
   `cargo test -p bbnf-bench lock14_baseline -- --nocapture` passed, and the
   targeted frozen-surface diff over grammar, runtime, IR, passes, codegen,
   SIMD, direct, Track 2, parity, scan, materialization, real typed source, and
   RESULTS paths was empty.

5. No grammar-neutrality or generic crate leakage blocker is present. The only
   code change is in the JSON bench report gate, which is already a telemetry
   gate surface. No generic runtime, IR, pass, codegen, SIMD, grammar, BBNF, or
   parse-that-regex crate was edited, and the targeted scan found the new
   Apache/CITM/real-typed baseline logic only in bench/report surfaces.

6. The W0 run-id strict validator remains intact. The standard checked report
   path now renders rows and then fails at the known W0 run-id drift:
   `json/twitter/parse_only/main` moved from
   `sk-v8-open:criterion-fnv64-9a37562ed3d0383a` to
   `sk-v8-open:criterion-fnv64-b9435757f85b6da0`. It no longer fails first on
   missing Apache/CITM `real_typed_struct` metadata, which is the intended V4
   distinction.

## Verification

- `cargo test -p bbnf-bench w0_real_typed_metadata_expectation_uses_measured_baseline_not_source_fixtures -- --nocapture`: PASS.
- `cargo test -p bbnf-bench lock14_baseline -- --nocapture`: PASS.
- `cargo test -p bbnf-bench report::tests::w0_report_accepts_exact_opening_baseline -- --nocapture`: PASS.
- `cargo xtask check-real-typed`: PASS.
- `cargo xtask gate-json --advisory --check-results`: expected FAIL at W0 run-id strict drift; no Apache/CITM real-typed metadata failure observed.
- `git diff --check 74fe4e1b^ 74fe4e1b --`: PASS.
- `git diff --exit-code 74fe4e1b^ 74fe4e1b --` over `skinny/RESULTS.md`, grammar inputs, runtime, IR, passes, codegen, grammar/BBNF crates, SIMD, parse-that-regex, direct, Track 2, parity, scan, materialization, real typed owner paths, and `xtask/src/real_typed_schema.rs`: PASS.
- `rg` over `skinny/RESULTS.md`: only `twitter`, `update_center`, `mesh`, and `marine_ik` appear as measured `real_typed_struct` rows.

## Required Folds

None. Preserve the current split: Apache/CITM remain W2 source/product parity
fixtures only, benchmark row-table admission remains routed, and the W0 run-id
strict validator should not be weakened to make local drift pass.
