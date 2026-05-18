# SK-V8 W2 Research: Typed Product Plane Expansion

Date: 2026-05-18.
Scope: W2 typed product-plane expansion after W1 CostFacts gate closure.

## Findings

W2 is now dispatchable only as a typed product-plane wave. W0 and W1 are closed,
but parser/runtime substrate, direct digest guard rows, generic CostFacts, and
Lock 14 frozen roots remain out of scope unless a W2 plan names and challenges
them. The admissible source/product proof for this slice is generated Track 1
typed DirectBuild plus serde_json as the Track 2/oracle path and a separate
sonic-rs typed parity lane. The existing `track2_typed` helper delegates to
serde_json; W2 does not claim a third independent typed parser beyond serde and
sonic.

The smallest candidate set is `apache_builds` plus `citm_catalog`.

- `apache_builds` is an object root with root `mode`, root `nodeName`, and a
  stable `jobs` array of 875 Jenkins job records. The admitted schema consumes
  `mode`, `nodeName`, `jobs[].name`, `jobs[].url`, and `jobs[].color`; other
  root fields remain skipped by the existing unknown-field policy.
- `canada` was falsified during W2 pre-redress: a full-fixture parity check
  exposed generated DirectBuild vs serde checksum divergence on long decimal
  coordinate payloads. It is routed out of W2 rather than weakening typed
  equality to length-only proof.
- `citm_catalog` is an object root with event maps. A useful schema can consume
  `events` as keyed entries with `id`, `name`, `subTopicIds`, and `topicIds`,
  staying on string/u64/vector product data and avoiding the Canada float
  materialization mismatch.

Both candidates use existing fixture facts, existing DirectBuild schema
machinery, and existing benchmark loops. They do not require a new directive,
BIR variant, `BackendShape`, public substrate API, sidecar index, or parser-owned
structural cursor.

## Owner Paths

- `skinny/xtask/src/real_typed_schema.rs`
- `skinny/crates/bbnf-bench/src/real_typed_struct.rs`
- `skinny/crates/bbnf-bench/src/generated_real_typed.rs` generated output
- `skinny/RESULTS.md` only if the W2 row gate is intentionally refreshed
- `skinny/REDRESS.md` only if a W2 row rejects or routes

## Gates

- `cargo xtask regen-real-typed`
- `cargo xtask check-real-typed`
- `cargo test -p bbnf-bench real_typed -- --nocapture`
- `cargo xtask check-json`
- `cargo xtask check-conformance`
- Generated diff limited to `generated_real_typed.rs`.
- No runtime/parser/product guard drift outside W2 typed files.

Full benchmark admission remains row-gated. If local Criterion drift blocks the
W0 baseline validator again, W2 records source/product parity only and rejects
benchmark row-table admission for this wave rather than hiding it in the typed
schema commit.
