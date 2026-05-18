# CH1 - SK-V8 W2 Typed Product-Plane Hardening

Verdict: REVISE

Confidence: 88%

## Findings

1. Admission and close state drift from `RESULTS.md`. The current worktree marks W2 closed in `HANDOFF.md` and admits `apache_builds/real_typed_struct` plus `citm_catalog/real_typed_struct` in `REDRESS.md` while explicitly leaving `skinny/RESULTS.md` unchanged. That does not satisfy the close predicate that any W2-added typed row status agrees with W2 REDRESS and RESULTS, and `RESULTS.md` currently has only `citm_catalog` parse/direct rows and `apache_builds` parse/direct rows, not the new real-typed rows. Evidence: `restart/skinny/tranches/sk-v8/HANDOFF.md:5`, `restart/skinny/tranches/sk-v8/HANDOFF.md:134`, `restart/skinny/tranches/sk-v8/HANDOFF.md:174`, `restart/skinny/tranches/sk-v8/HANDOFF.md:180`, `skinny/REDRESS.md:2622`, `skinny/REDRESS.md:2646`, `skinny/RESULTS.md:8`, `skinny/RESULTS.md:9`, `skinny/RESULTS.md:12`, `skinny/RESULTS.md:13`, `restart/skinny/tranches/sk-v8/SPEC.md:737`, `restart/skinny/tranches/sk-v8/SPEC.md:738`.

2. Lock 14 admission is not green at commit `12aff1e4`. The reviewed source commit touches `crates/bbnf-bench/src/real_typed_struct.rs`, `crates/bbnf-bench/src/generated_real_typed.rs`, and `xtask/src/real_typed_schema.rs`; all three are in the Lock 14 frozen-root diff list. `cargo run -p xtask -- gate-json --check-results` failed before RESULTS comparison with `Lock 14 frozen diff failed: git diff --quiet HEAD^ -- ... real_typed_struct.rs ... generated_real_typed.rs ... real_typed_schema.rs`, and `cargo test -p bbnf-bench lock14_baseline -- --nocapture` failed the same way. Evidence: `skinny/crates/bbnf-bench/src/lock14_baseline.rs:375`, `skinny/crates/bbnf-bench/src/lock14_baseline.rs:399`, `skinny/crates/bbnf-bench/src/lock14_baseline.rs:419`, `skinny/crates/bbnf-bench/src/lock14_baseline.rs:439`, `restart/skinny/tranches/sk-v8/SPEC.md:478`, `restart/skinny/tranches/sk-v8/SPEC.md:740`.

3. The strict parity proof passes for Apache and CITM, but the Track 2 wording is overclaimed. The implementation's `track2_typed` is just `serde_typed(fixture, input.as_bytes())`, and `assert_real_typed_parity` then calls `track2_typed` and `serde_typed` separately, so the distinct strict engines are generated Track 1, serde_json, and sonic-rs; there is not an additional independent Track 2 implementation beyond serde. That is acceptable only if the fold names serde as the Track 2/oracle path instead of claiming independent Track 2 plus serde. Evidence: `skinny/crates/bbnf-bench/src/real_typed_struct.rs:251`, `skinny/crates/bbnf-bench/src/real_typed_struct.rs:258`, `skinny/crates/bbnf-bench/src/real_typed_struct.rs:310`, `skinny/REDRESS.md:2633`, `restart/skinny/tranches/sk-v8/SPEC.md:464`, `restart/skinny/tranches/sk-v8/SPEC.md:476`.

4. The source implementation itself stays inside the intended product-plane slice. The commit adds Apache and CITM roots to the real typed schema, generates only `generated_real_typed.rs`, routes fixture names for Apache/CITM but not Canada, and adds full-fixture parity tests for the two admitted candidates. It does not touch grammar, runtime JSON, BIR, directives, substrate, direct digest implementation, or fixture files. Evidence: `skinny/xtask/src/real_typed_schema.rs:22`, `skinny/xtask/src/real_typed_schema.rs:57`, `skinny/xtask/src/real_typed_schema.rs:75`, `skinny/crates/bbnf-bench/src/generated_real_typed.rs:53`, `skinny/crates/bbnf-bench/src/generated_real_typed.rs:64`, `skinny/crates/bbnf-bench/src/real_typed_struct.rs:182`, `skinny/crates/bbnf-bench/src/real_typed_struct.rs:610`.

5. Canada is correctly routed out of this source slice. There is no `RealTypedFixture::Canada`, no Canada schema root, and the worktree docs record the full-fixture DirectBuild-versus-serde checksum mismatch instead of weakening proof to length-only or digest-only evidence. Evidence: `skinny/crates/bbnf-bench/src/real_typed_struct.rs:10`, `skinny/crates/bbnf-bench/src/real_typed_struct.rs:182`, `skinny/xtask/src/real_typed_schema.rs:11`, `skinny/REDRESS.md:2635`, `restart/skinny/tranches/sk-v8/HANDOFF.md:177`.

## Verification

- PASS: `cargo run -p xtask -- check-real-typed`
- PASS: `cargo run -p xtask -- check-json`
- PASS: `cargo run -p xtask -- check-conformance`
- PASS: `cargo test -p bbnf-bench real_typed -- --nocapture`
- PASS: `cargo test -p codegen typed_direct -- --nocapture`
- PASS: `git diff --check 12aff1e4^ 12aff1e4`
- FAIL: `cargo test -p bbnf-bench lock14_baseline -- --nocapture`
- FAIL: `cargo run -p xtask -- gate-json --check-results`

## Required Fold

Fold before admission:

1. Either produce a clean measured W2 `RESULTS.md` refresh that adds `apache_builds/real_typed_struct` and `citm_catalog/real_typed_struct`, proves the declared same-plane thresholds and existing-row/no-regression floors, and reconciles HANDOFF/REDRESS/RESULTS; or revise the current HANDOFF/REDRESS edits to say W2 is not closed and `12aff1e4` is source/product parity only, with row admission deferred.
2. Resolve the Lock 14 gate mismatch for W2 typed-owner files. The fold must make `gate-json --check-results` and `lock14_baseline` pass, or explicitly route why the source slice is not yet admitted.
3. Correct the Track 2 wording: either add a genuinely separate typed Track 2/oracle, or state that serde_json is the Track 2/oracle path and remove the claim of independent Track 2 plus serde.

Source commit `12aff1e4` should not be admitted as a closed W2 row-admission commit without this fold. As code-only W2 product-plane implementation, it is close, but the current worktree admission docs and gates are not coherent enough for ACCEPT.
