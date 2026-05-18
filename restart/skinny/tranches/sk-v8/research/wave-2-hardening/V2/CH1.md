# SK-V8 W2 Hardening V2 CH1

Reviewed target: `8ce03af4` (`fix(sk-v8-wave2-gate): fold typed hardening disposition`).

Verdict: ACCEPT

Confidence: 94%

## Findings

1. Lock 14's V1 parent-diff blocker is folded. `FROZEN_ROOTS` still covers the
   real typed owner files plus the off-scope runtime/parser/substrate/direct,
   Track 2, parity, scan, and materialization roots
   (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:375`). W2 parent-diff
   authorization is limited to commits whose subject contains `sk-v8-wave2` and
   whose frozen-root parent diff is confined to the three real typed owner paths:
   `generated_real_typed.rs`, `real_typed_struct.rs`, and
   `xtask/src/real_typed_schema.rs`
   (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:399`,
   `skinny/crates/bbnf-bench/src/lock14_baseline.rs:455`). The positive and
   negative tests cover owner-only admission and runtime-generated rejection
   (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:618`,
   `skinny/crates/bbnf-bench/src/lock14_baseline.rs:632`).

2. The generated schema identity and Apache host/API facts are folded. The schema
   source and generated header now identify the W2 row set as
   `sk-v8-real-typed-w2` (`skinny/xtask/src/real_typed_schema.rs:10`,
   `skinny/crates/bbnf-bench/src/generated_real_typed.rs:3`). Apache root
   `mode`, root `nodeName`, and job string fields are present in schema, plan,
   research, checksum, and REDRESS surfaces
   (`skinny/xtask/src/real_typed_schema.rs:57`,
   `restart/skinny/tranches/sk-v8/research/skv8-W2-plan.md:11`,
   `restart/skinny/tranches/sk-v8/research/skv8-W2-typed-product-expansion.md:19`,
   `skinny/crates/bbnf-bench/src/real_typed_struct.rs:351`,
   `skinny/REDRESS.md:2632`).

3. The Track 2/oracle wording now matches the implementation. `track2_typed`
   still delegates to `serde_typed`
   (`skinny/crates/bbnf-bench/src/real_typed_struct.rs:251`), and the live W2
   surfaces now call serde_json the Track 2/oracle path with a separate sonic-rs
   checksum lane, not a third independent typed parser
   (`restart/skinny/tranches/sk-v8/research/skv8-W2-typed-product-expansion.md:11`,
   `restart/skinny/tranches/sk-v8/research/skv8-W2-plan.md:41`,
   `restart/skinny/tranches/sk-v8/HANDOFF.md:175`,
   `skinny/REDRESS.md:2635`).

4. REDRESS 91, the no-RESULTS posture, and Canada route-out are folded. REDRESS
   states that `apache_builds/real_typed_struct` and
   `citm_catalog/real_typed_struct` are admitted source/product rows, not
   measured rows in the current W0 `skinny/RESULTS.md` manifest
   (`skinny/REDRESS.md:2622`). It rejects `canada/real_typed_struct` on the
   full-fixture DirectBuild-versus-serde checksum mismatch and blocks weakened
   length-only or digest-only proof (`skinny/REDRESS.md:2637`). It also states
   that `skinny/RESULTS.md` is unchanged and W2 rejects benchmark row-table
   admission for this wave (`skinny/REDRESS.md:2648`). HANDOFF agrees:
   W2 is source/product parity admitted, benchmark row-table admission is
   rejected/routed, and the current manifest remains four measured
   `real_typed_struct` rows (`restart/skinny/tranches/sk-v8/HANDOFF.md:5`,
   `restart/skinny/tranches/sk-v8/HANDOFF.md:175`). The current RESULTS table
   still has only `twitter`, `update_center`, `mesh`, and `marine_ik`
   `real_typed_struct` rows (`skinny/RESULTS.md:7`, `skinny/RESULTS.md:18`,
   `skinny/RESULTS.md:21`, `skinny/RESULTS.md:28`).

5. Focused verification supports the fold. `cargo test -p bbnf-bench
   lock14_baseline -- --nocapture` passed all 10 Lock 14 tests. `cargo xtask
   check-real-typed` passed. `cargo test -p bbnf-bench real_typed --
   --nocapture` passed all 7 real typed tests, including the full Apache/CITM
   fixture parity test (`skinny/crates/bbnf-bench/src/real_typed_struct.rs:610`).
   `cargo test -p codegen typed_direct -- --nocapture`, `cargo xtask
   check-json`, and `cargo xtask check-conformance` passed. `git diff
   --exit-code HEAD^ HEAD -- skinny/RESULTS.md` had no diff, and a targeted
   off-scope diff over runtime, IR, passes, codegen, grammar, bbnf, SIMD,
   parse-that-regex, direct, Track 2, parity, scan, materialization, and RESULTS
   paths was empty.

## Required Folds

None.
