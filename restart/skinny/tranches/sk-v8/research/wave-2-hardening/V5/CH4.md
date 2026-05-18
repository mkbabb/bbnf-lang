# SK-V8 W2 Hardening V5 - CH4

Date: 2026-05-18.
Reviewer: CH4.
Target reviewed: `bf2f073d`
(`docs(sk-v8-wave2-hardening): record V4 accept cycle`).

## Verdict

ACCEPT.

Confidence: 95%.

## Findings

1. The V4-folded executable target is unchanged at HEAD. The diff from
   `74fe4e1b` to `bf2f073d` contains only V4 hardening evidence files under
   `restart/skinny/tranches/sk-v8/research/wave-2-hardening/V4/`. There is no
   source, `skinny/RESULTS.md`, `skinny/REDRESS.md`, gate, Lock 14, generated
   typed, or schema delta in the V5 target. This means V5 is a re-challenge of
   the same V4-folded gate and typed-product surface, plus the V4 accept record.

2. The V3 checked-report blocker remains folded without admitting Apache/CITM
   measured benchmark rows. `validate_w0_capture_metadata` receives
   `w0_real_typed_metadata_expected(&fixture.name)`, and that helper derives
   the real-typed metadata requirement from
   `sk_v8_open_baseline("json/{fixture}/real_typed_struct/main")`, not from the
   broader W2 source fixture map. The regression test still asserts `twitter`
   and `update_center` are expected, while source-only `apache_builds` and
   `citm_catalog` are not. `required_metadata_specs` appends
   `track1_real_typed_struct`, `track2_real_typed_struct`,
   `sonic_rs_real_typed_struct`, and `serde_json_real_typed_struct` only when
   that measured-baseline predicate is true.

3. Row-table admission remains frozen to the W0 measured shape. `skinny/RESULTS.md`
   still contains exactly four measured `real_typed_struct` rows: `twitter`,
   `update_center`, `mesh`, and `marine_ik`. There are no measured
   `apache_builds/real_typed_struct`, `citm_catalog/real_typed_struct`, or
   `canada/real_typed_struct` rows. The live Criterion tree also has no
   Apache/CITM `real_typed_struct` metadata directories, so the V3 failure mode
   is not being hidden by local artifacts.

4. Lock 14 and the frozen surface hold. `FROZEN_ROOTS` still covers grammar,
   fixtures, runtime, IR, passes, codegen, grammar/bbnf crates, SIMD,
   parse-that-regex, direct struct, real typed owner files, Track 2, parity,
   scan, materialization, and `xtask/src/real_typed_schema.rs`. The W2 parent
   allowance remains confined to the three typed owner paths and only under an
   `sk-v8-wave2` subject; the test coverage still includes acceptance of those
   owner paths, rejection of out-of-owner runtime movement, path normalization,
   and coverage of directive/ASM surfaces.

5. Grammar-neutrality and generic crate leakage checks are clean. The broader
   W2 touched-path set since the typed source commit is limited to W2 planning
   and hardening docs, `skinny/REDRESS.md`, the report gate, Lock 14, and the
   authorized real-typed owner files. A targeted diff over grammar inputs,
   runtime, IR, passes, codegen, grammar/bbnf crates, SIMD, parse-that-regex,
   direct struct, Track 2, parity, scan, materialization, and `skinny/RESULTS.md`
   is empty. No generic parser/runtime/substrate or grammar behavior is part of
   this V5 target.

6. No parser/runtime/substrate/direct drift is present. Apache/CITM remain
   typed product-plane fixtures only: generated Track 1 DirectBuild,
   serde_json-backed Track 2/oracle, and a separate sonic-rs strict checksum
   lane. `track2_typed` still delegates to `serde_typed`, and
   `assert_real_typed_parity` checks Track 1, Track 2/serde, and sonic
   checksums. Canada remains routed out on the full-fixture checksum mismatch;
   no fold weakens that to length-only or digest-only evidence.

7. Strict-vs-strict comparator discipline has not regressed. `report.rs` still
   requires W0 row count and row-id identity, exact `SK_V8_OPEN_RUN_ID`, stable
   outcomes/verdicts, and bounded Track 1/Track 2 baseline deltas. Native
   `sonic_rs_strict` and `serde_json` comparators remain strict,
   same-run-native, on the expected output plane, with expected Criterion
   source artifacts. Lossy sonic stays a parse-only permissive flaw probe, and
   sidecar comparators remain historical or absent slots.

## Verification

- `git rev-parse HEAD`: `bf2f073d99309c84adf0dd3770e5071778422125`.
- `git status --short`: clean before creating this owned file.
- `git diff --name-only --no-renames 74fe4e1b..bf2f073d`: only V4 CH files and
  `HARDENING-W2-V4-CONSOLIDATED.md`.
- `git diff --exit-code 74fe4e1b..bf2f073d -- skinny/RESULTS.md skinny/REDRESS.md skinny/crates/bbnf-bench/src/bin/gate.rs skinny/crates/bbnf-bench/src/report.rs skinny/crates/bbnf-bench/src/real_typed_struct.rs skinny/crates/bbnf-bench/src/generated_real_typed.rs skinny/xtask/src/real_typed_schema.rs skinny/crates/bbnf-bench/src/lock14_baseline.rs`:
  PASS, no source or row-table diff after V4.
- `git diff --name-only --no-renames 12aff1e4^..bf2f073d`: W2 code movement is
  limited to `gate.rs`, `lock14_baseline.rs`, the three real-typed owner files,
  `skinny/REDRESS.md`, HANDOFF/planning docs, and challenge evidence.
- `git diff --exit-code 12aff1e4^..bf2f073d -- skinny/grammars skinny/test_data skinny/crates/test-fixtures skinny/crates/runtime/src skinny/crates/ir/src skinny/crates/passes/src skinny/crates/codegen/src skinny/crates/grammar/src skinny/crates/bbnf/src skinny/crates/bbnf-simd/src skinny/crates/bbnf-simd/build.rs skinny/crates/bbnf-simd/ext skinny/crates/parse-that-regex/src skinny/crates/bbnf-bench/src/direct_struct.rs skinny/crates/bbnf-bench/src/track2 skinny/crates/bbnf-bench/src/parity.rs skinny/crates/bbnf-bench/src/scan.rs skinny/crates/bbnf-bench/src/materialization.rs skinny/RESULTS.md`:
  PASS, no off-scope frozen-surface or RESULTS drift.
- `git diff --check 12aff1e4^..bf2f073d`: PASS.
- `awk` audit of `skinny/RESULTS.md`: measured `real_typed_struct` rows are
  only `twitter`, `update_center`, `mesh`, and `marine_ik`.
- `find skinny/target/criterion -path '*apache_builds*real_typed_struct*' -o -path '*citm_catalog*real_typed_struct*'`:
  no Apache/CITM real-typed Criterion metadata directories found.
- `rg`/`sed` review of `gate.rs`, `lock14_baseline.rs`, `report.rs`, and
  `real_typed_struct.rs` confirmed the measured-baseline metadata predicate,
  scoped Lock 14 W2 allowance, strict native comparator validation, and
  Track 2/serde plus separate sonic strict parity shape.

I did not run cargo tests for V5 because the assignment allowed exactly one
file to be written; running cargo would create or mutate build artifacts. The
V4 record cites the executable test evidence for the unchanged target, and V5
adds no source change that would invalidate that evidence.

## Required Folds

None. Preserve the current split: Apache/CITM remain W2 source/product parity
fixtures only, Canada remains routed out, benchmark row-table admission remains
rejected for this wave, and the W0 strict run-id/comparator validators must not
be weakened to make local drift pass.
