# SK-V8 W2 Research: Typed Product Plane Expansion

Date: 2026-05-18.
Scope: W2 typed product-plane expansion after W1 CostFacts gate closure.

## Findings

W2 is now dispatchable only as a typed product-plane wave. W0 and W1 are closed,
but parser/runtime substrate, direct digest guard rows, generic CostFacts, and
Lock 14 frozen roots remain out of scope unless a W2 plan names and challenges
them. The admissible product proof is generated Track 1 typed DirectBuild plus
independent Track 2, serde, and sonic typed parity.

The smallest candidate set is `apache_builds` plus `canada`.

- `apache_builds` is an object root with a stable `jobs` array of 875 Jenkins
  job records. A useful schema only needs `jobs[].name`, `jobs[].url`, and
  `jobs[].color`; other root fields can be skipped by the existing unknown-field
  policy.
- `canada` is an object-root GeoJSON fixture with one feature and a polygon
  coordinate plane. A useful schema can consume `type`, `features[].type`,
  `features[].geometry.type`, and `coordinates: Vec<Vec<Vec<f64>>>`.

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
W0 baseline validator again, W2 must record the benchmark evidence and route the
RESULTS update explicitly rather than hiding it in the typed schema commit.
