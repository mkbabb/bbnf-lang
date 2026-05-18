# SK-V8 W2 Plan: Typed Product Plane Expansion

Date: 2026-05-18.
Authority: W0 and W1 closed; W2 research selected `apache_builds`; W2
pre-redress falsified `canada` and routes it out in favor of `citm_catalog`.

## Plan

Implement exactly two new `real_typed_struct` fixtures:

1. `apache_builds`: generated typed parser `parse_apache_builds` over root
   `ApacheBuilds<'i>`, consuming root `mode`, root `nodeName`,
   `jobs[].name`, `jobs[].url`, and `jobs[].color`.
2. `citm_catalog`: generated typed parser `parse_citm_catalog` over root
   `CitmCatalog<'i>`, consuming `events` as keyed entries and selected event
   fields.

No parser/runtime/tape/direct-digest/substrate source is in scope. W2 may edit
only:

- `skinny/xtask/src/real_typed_schema.rs`
- `skinny/crates/bbnf-bench/src/real_typed_struct.rs`
- `skinny/crates/bbnf-bench/src/generated_real_typed.rs`
- `skinny/RESULTS.md` and `skinny/REDRESS.md` only for measured row disposition
  after source/generation passes

## Gates

Implementation gates:

- `cargo xtask regen-real-typed`
- `cargo xtask check-real-typed`
- `cargo test -p bbnf-bench real_typed -- --nocapture`
- `cargo xtask check-json`
- `cargo xtask check-conformance`
- Diff freeze for runtime JSON, Track 2, direct digest, scan, parity, and
  materialization helpers.

Admission gates:

- New Track 1 typed rows parse and checksum-match the serde_json-backed
  Track 2/oracle path and the separate sonic-rs strict lane on both minimal
  tests and full fixture payloads.
- Existing `twitter`, `update_center`, `mesh`, and `marine_ik` typed parity
  remains green.
- If a W2 benchmark refresh is attempted, record whether the standard W0
  validator accepts the refreshed report. If it rejects run-id or throughput
  drift unrelated to W2 source, keep `RESULTS.md` unchanged, reject benchmark
  row-table admission for W2, and record the source/product parity evidence
  explicitly instead of weakening W0 validation.

## Revert Protocol

Revert W2 by reverting one source/generated redress commit and, if present, one
separate RESULTS/REDRESS status commit. The source commit must not include
parser/runtime/product-guard drift outside the typed DirectBuild files.
