# CH3 - SK-V8 W2 Hardening Review

Reviewed source commit: `12aff1e4` (`feat(sk-v8-wave2-typed): add Apache and CITM typed product rows`).

Verdict: REVISE
Confidence: 92%

## Findings

1. Blocking: Lock 14's current frozen-surface test fails on the W2 source
   commit. `cargo test -p bbnf-bench lock14_baseline -- --nocapture` fails
   `lock14_baseline::tests::accepts_current_allowlist` because
   `validate_git_freeze` runs `git diff --quiet HEAD^ -- <FROZEN_ROOTS>`, and
   W2 intentionally changed three paths that are currently in `FROZEN_ROOTS`:
   `crates/bbnf-bench/src/generated_real_typed.rs`,
   `crates/bbnf-bench/src/real_typed_struct.rs`, and
   `xtask/src/real_typed_schema.rs`. This is not parser/runtime/substrate
   drift, but it means the frozen-surface gate does not yet distinguish
   wave-authorized typed product movement from forbidden Lock 14 movement.

2. The no-new-directive/BIR/substrate boundary is otherwise clean. The source
   commit touches only the W2 typed schema source, typed carriers/tests, and the
   generated real-typed output. A targeted diff over `grammars`, runtime,
   IR/passes/codegen/grammar/bbnf/SIMD/parse-that-regex, direct-struct,
   Track 2, parity, scan, and materialization paths was empty. No new
   directive, BIR variant, `BackendShape`, substrate API, parser-owned cursor,
   sidecar substrate, or direct digest route is introduced.

3. Generated-output ownership is mostly proven, but the provenance label needs
   a fold. `cargo xtask check-real-typed` passes, so
   `generated_real_typed.rs` is reproducible from `xtask/src/real_typed_schema.rs`.
   However both schema source and generated header still advertise
   `schema_hash: sk-v7-real-typed-v2` after adding W2 roots. If this field is
   intended as schema identity, it is now stale; if it is only a compatibility
   tag, the W2 closure notes need to say so explicitly.

4. Apache and CITM fit the W2 product plane without parser/runtime drift.
   `apache_builds` and `citm_catalog` use existing DirectBuild typed schema
   machinery and existing serde/sonic oracles; `cargo test -p bbnf-bench
   real_typed -- --nocapture` passes the minimal and full-fixture W2 parity
   tests plus the four existing real-typed rows. CITM stays on keyed event maps,
   strings, `u64`, and vectors, avoiding the Canada float-materialization
   failure class.

5. Apache's product projection is slightly wider than the W2 plan text. The
   plan names `jobs[].name`, `jobs[].url`, and `jobs[].color`, but the commit
   also carries root `mode` and `nodeName` through typed structs, generated
   parsing, serde/sonic parity, and checksums. That is still product-plane data,
   but the exact host/API schema facts are not fully reflected in the plan.

6. The remaining implementation gates I reran are green:
   `cargo xtask check-real-typed`, `cargo test -p bbnf-bench real_typed -- --nocapture`,
   `cargo xtask check-json`, `cargo xtask check-conformance`, and
   `git diff --check 12aff1e4^ 12aff1e4`.

## Required Folds

1. Fold Lock 14 accounting for W2-authorized typed product movement. Either make
   the Lock 14 test accept an explicit wave-authorized owner-path allowance while
   still freezing generic/parser/runtime/substrate/direct surfaces, or replace
   the parent-diff assertion with a W2-specific frozen-surface check and record
   that command in the closure evidence. The focused Lock 14 test or its named
   replacement must be green before W2 hardening closes.

2. Resolve generated-output provenance. Rename `schema_hash` to a W2-specific
   schema identity, replace it with a deterministic schema digest, or document
   why `sk-v7-real-typed-v2` is intentionally a compatibility tag rather than a
   changed-schema identity. Regenerate and re-run `cargo xtask check-real-typed`
   if the source changes.

3. Fold the Apache host/API schema facts. Either update W2 plan/closure notes to
   name root `mode` and `nodeName` as admitted product fields, or remove those
   fields from the W2 schema/carriers/checksum/generated output so the source
   exactly matches the plan.

4. Preserve the current source boundary through any fold: no changes to generic
   parser/runtime/tape/substrate, BIR, directives, direct digest, Track 2,
   parity, scan, or materialization paths. Re-run the targeted off-scope diff
   after folding.

5. Keep Canada routed out and keep `RESULTS.md` out of the W2 source slice
   unless a separate measured row-disposition commit passes the W0 validator.
