# SK-V8 W2 Hardening V3 CH1

Reviewed target: `8ce03af4`
(`fix(sk-v8-wave2-gate): fold typed hardening disposition`).

Verdict: ACCEPT

Confidence: 95%

## Challenge

The V2 ACCEPT posture still holds at current HEAD.

1. W2 is admitted only as source/product parity. `HANDOFF.md` records W2 as
   "source/product parity admitted, benchmark row-table admission rejected for
   this wave" and keeps W3 as the next dispatchable wave
   (`restart/skinny/tranches/sk-v8/HANDOFF.md:5-8`). REDRESS 91 admits only
   `apache_builds/real_typed_struct` and `citm_catalog/real_typed_struct` as
   source/product rows and explicitly says they are not current measured rows
   (`skinny/REDRESS.md:2622-2625`).

2. Benchmark row-table admission is still rejected, not deferred or papered
   over. The W2 plan says a failed refresh must leave `skinny/RESULTS.md`
   unchanged, reject W2 benchmark row-table admission, and record only
   source/product parity evidence
   (`restart/skinny/tranches/sk-v8/research/skv8-W2-plan.md:46-50`). REDRESS 91
   takes exactly that route because local Criterion metadata drift trips the W0
   run-id validator, and states W2 does not claim six measured
   `real_typed_struct A / GO` rows (`skinny/REDRESS.md:2648-2652`).

3. The product proof still matches the declared W2 surface. The schema identity
   is `sk-v8-real-typed-w2` in both schema source and generated output
   (`skinny/xtask/src/real_typed_schema.rs:7-10`;
   `skinny/crates/bbnf-bench/src/generated_real_typed.rs:1-4`). Apache consumes
   root `mode`, root `nodeName`, and job string fields; CITM consumes keyed
   event entries with `id`, `name`, `subTopicIds`, and `topicIds`
   (`skinny/xtask/src/real_typed_schema.rs:57-98`). The checksum path folds the
   same fields, and the W2 full-fixture parity test covers Apache and CITM
   (`skinny/crates/bbnf-bench/src/real_typed_struct.rs:351-384`,
   `skinny/crates/bbnf-bench/src/real_typed_struct.rs:609-618`).

4. The Track 2/oracle wording is still honest. `track2_typed` delegates to
   `serde_typed`, `serde_typed` uses serde_json, and sonic-rs is a separate
   checksum parity lane (`skinny/crates/bbnf-bench/src/real_typed_struct.rs:251-323`).
   The W2 research and plan now say the same thing rather than claiming a third
   independent typed parser
   (`restart/skinny/tranches/sk-v8/research/skv8-W2-typed-product-expansion.md:11-15`;
   `restart/skinny/tranches/sk-v8/research/skv8-W2-plan.md:41-43`).

5. Lock 14 remains a scoped owner allowance, not a broad freeze bypass.
   `FROZEN_ROOTS` still covers directive, grammar, runtime, IR, passes, codegen,
   SIMD, parser, direct, Track 2, parity, scan, materialization, generated typed,
   real typed, and schema roots
   (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:375-397`). The W2 parent
   diff allowance is limited to `sk-v8-wave2` subjects and the three typed owner
   paths: `generated_real_typed.rs`, `real_typed_struct.rs`, and
   `xtask/src/real_typed_schema.rs`
   (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:399-470`). Tests cover W2
   owner admission, non-W2 rejection, path normalization, and W2-scoped
   out-of-owner rejection (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:618-652`).

6. `skinny/RESULTS.md` has no W2 drift. `git diff --exit-code HEAD^ HEAD --
   skinny/RESULTS.md` was empty, and the row audit still finds only four
   measured `real_typed_struct` rows: `twitter`, `update_center`, `mesh`, and
   `marine_ik` (`skinny/RESULTS.md:7`, `skinny/RESULTS.md:18`,
   `skinny/RESULTS.md:21`, `skinny/RESULTS.md:28`). There are no measured
   `apache_builds`, `citm_catalog`, or `canada` `real_typed_struct` rows.

7. No directive/BIR/substrate/runtime/direct drift was found. The targeted
   `12aff1e4^..HEAD` diff over grammar/directive roots, fixtures, runtime, IR,
   passes, codegen, grammar crate, bbnf crate, SIMD, parse-that-regex, direct
   struct, Track 2, parity, scan, materialization, and `skinny/RESULTS.md` is
   empty. The only W2-to-HEAD source movement is in the typed owner paths plus
   Lock 14 gate hardening and W2 disposition docs.

## Verification

- `cargo xtask check-real-typed`
- `cargo test -p bbnf-bench real_typed -- --nocapture`
- `cargo test -p bbnf-bench lock14_baseline -- --nocapture`
- `cargo test -p codegen typed_direct -- --nocapture`
- `cargo xtask check-json`
- `cargo xtask check-conformance`
- `git diff --check HEAD^ HEAD`
- `git diff --exit-code HEAD^ HEAD -- skinny/RESULTS.md`
- Targeted off-scope drift diff over directive/BIR/substrate/runtime/direct
  surfaces from `12aff1e4^..HEAD`

All verification passed.

## Required Folds

None.
