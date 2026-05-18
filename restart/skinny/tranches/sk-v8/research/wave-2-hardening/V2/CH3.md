# CH3 - SK-V8 W2 Hardening V2 Review

Reviewed HEAD: `8ce03af4`
(`fix(sk-v8-wave2-gate): fold typed hardening disposition`).

Verdict: ACCEPT
Confidence: 94%

## Findings

1. The executable gate set is green. I reran
   `cargo test -p bbnf-bench lock14_baseline`,
   `cargo xtask check-real-typed`,
   `cargo test -p bbnf-bench real_typed -- --nocapture`,
   `cargo xtask check-json`, and `cargo xtask check-conformance` from
   `skinny/`. Lock 14 passed 10 tests. The real-typed suite passed seven tests:
   the four existing typed rows, the two W2 source/product rows, and the W2
   full-fixture parity test. Conformance accepted 21 valid fixtures and rejected
   seven invalid fixtures. I also reran the claimed supporting codegen gate,
   `cargo test -p codegen typed_direct -- --nocapture`, and `git diff --check
   HEAD^ HEAD`; both passed.

2. The V1 blocking Lock 14 issue is folded. `validate_git_freeze` now computes
   the parent diff under `FROZEN_ROOTS` and authorizes only W2-scoped commit
   subjects whose changed frozen paths are confined to the real-typed owner
   set: `crates/bbnf-bench/src/generated_real_typed.rs`,
   `crates/bbnf-bench/src/real_typed_struct.rs`, and
   `xtask/src/real_typed_schema.rs`. The focused tests cover W2 admission,
   non-W2 rejection, outside-path rejection, and repo-root path normalization.

3. Generated typed provenance is no longer stale. Both
   `skinny/xtask/src/real_typed_schema.rs` and generated
   `skinny/crates/bbnf-bench/src/generated_real_typed.rs` now use
   `sk-v8-real-typed-w2`, and `cargo xtask check-real-typed` proves the
   generated file matches the schema source.

4. The V1 documentation folds landed. W2 text now names Apache root `mode` and
   `nodeName`, states that `track2_typed` is serde_json-backed rather than a
   third independent parser, keeps Canada routed out, and records W2 as
   source/product parity only with benchmark row-table admission rejected for
   this wave. `skinny/RESULTS.md` remains unchanged and still reports the W0
   four measured `real_typed_struct A / GO` rows, not six.

5. No directive/BIR/substrate/runtime/direct drift was found. A targeted
   `HEAD^..HEAD` diff over grammar/directive roots, IR, passes, codegen,
   runtime, substrate-adjacent paths, direct digest, Track 2, parity, scan,
   materialization, and `skinny/RESULTS.md` is empty. The only frozen-root
   movement in this V2 fold is the authorized typed-owner schema/hash movement;
   `skinny/crates/bbnf-bench/src/lock14_baseline.rs` is gate-surface hardening.

## Required Folds

None.
