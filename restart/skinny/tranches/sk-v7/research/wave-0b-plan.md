# SK-V7 Wave 0b Plan: schema-v3 telemetry row builder

Inputs: `restart/skinny/tranches/sk-v7/SPEC.md` §0.2-§0.3 and §2,
`restart/prompts/pass-contracts/PASS-ALPHA.md` §4.3,
`restart/skinny/tranches/sk-v7/HANDOFF.md` §3,
`restart/skinny/tranches/sk-v7/research/wave-0b-r1-report-schema.md`,
`restart/skinny/tranches/sk-v7/research/wave-0b-r2-sonic-provenance.md`,
and `restart/skinny/tranches/sk-v7/research/wave-0b-r3-contract.md`.

Intervention: replace the legacy report renderer with a generated schema-v3
telemetry row surface, bump Criterion metadata to v3, and make sonic-rs
strict/lossy provenance explicit without reintroducing the `utf8_lossy` Cargo
feature.

## Owner Paths

- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/crates/bbnf-bench/src/metadata.rs`
- `skinny/crates/bbnf-bench/benches/json_parity.rs`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`
- `restart/skinny/tranches/sk-v7/research/wave-0b-schema-v3-close.md`

## Implementation Shape

- Add one schema-v3 telemetry row type that renders the PASS-ALPHA §4.3 named
  columns for parse and workload rows. Parse rows use `Workload=parse_only`.
- Keep `Masking Probes` outside the telemetry table.
- Bump `RowMetadata::SCHEMA_VERSION` to `3` and record workload, strictness,
  UTF-8 boundary, flaw-probe, output plane, feature mask, API symbol, sidecar
  freshness, primitive status, and hot-leaf provenance.
- Repurpose the duplicate `sonic_rs_checked` benchmark into a same-run
  explicit lossy flaw-probe row using `Deserializer::utf8_lossy()`. Keep
  `sonic_rs_anchor` as the strict `from_slice` S-anchor candidate.
- Add parse `serde_json` to the generated telemetry surface from the existing
  Criterion row.
- Do not relabel Rust `simd-json` rows as C++ simdjson DOM/On Demand. Populate
  C++ sidecar columns only from documented sidecar artefacts and disclose them
  as stale profile data in row signals/hot-leaf provenance.

## Falsifiability Gate

- `cargo test -p bbnf-bench` passes, including schema-v3 header and missing
  required-field tests.
- `cargo tree -p bbnf-bench --edges=features | rg 'sonic-rs|utf8_lossy|sort_keys'`
  still shows `sonic-rs` with `sort_keys` only and no `utf8_lossy`.
- `cargo bench -p bbnf-bench --bench json_parity` regenerates v3 Criterion
  metadata, including strict and explicit-lossy sonic rows.
- `cargo run -p bbnf-bench --bin gate --release` regenerates `skinny/RESULTS.md`
  with PASS-ALPHA §4.3 named columns and schema validation before write.
- `cargo run -p xtask --release -- gate-json` reaches the same schema gate and
  fails only for the current measured performance verdict, not for missing
  schema-v3 columns.
- `sonic-rs strict Mbps` is populated from the strict post-W0 dependency.
  `sonic-rs lossy Mbps` is populated from same-run `Deserializer::utf8_lossy()`
  and never used for classification.
- `instruments` and `unicode_basic` remain measurement-classified; schema
  reshaping alone must not reclassify them to PASS.

## Hard Cap

W0b redress cap: 90 minutes total, split 45 minutes implementation, 25 minutes
focused tests/checks, and 20 minutes bench/gate/report close. If the bench
exceeds the cap, commit the schema implementation only if focused tests and the
feature-tree gate are green, then record the bench miss as W0b rejected.

## Revert Protocol

If schema-v3 rendering fails focused tests, revert the W0b source patch before
commit and record a W0b rejection with the failing test output.

If the explicit lossy row reintroduces `utf8_lossy` at the Cargo feature level,
revert the lossy row and keep only strict schema-v3 rendering; record the
feature-taint failure in REDRESS.

If the full bench/gate run fails because old metadata remains or required
columns are missing, save the patch to `/tmp/skv7-wave-0b-rejected.patch`, keep
`RESULTS.md` at the last generated authority, and record the next candidate:
schema-v3 metadata reader hardening.

## Same-Wave Consumer

The same-wave consumer is the existing `cargo run -p bbnf-bench --bin gate`
path, reached directly and through `cargo run -p xtask --release -- gate-json`.
The schema-v3 row builder is consumed by the gate in the same redress commit.

## Pre-Blocked Routes

This wave must not reopen parser/runtime performance routes from
`restart/skinny/tranches/sk-v7/HANDOFF.md` §3. In particular, it does not touch
Class A tiny-string wiring rejected by REDRESS 28 and 33, SK-V5 UTF-8 fusion
routes rejected by REDRESS 50-55, or SK-V6 retained/direct materialization
routes rejected by REDRESS 60-72.

## W1 Boundary

W1 may open only after W0b either admits schema-v3 telemetry with strict sonic
provenance or rejects with measurement evidence and a named successor. A W0b
schema table is not a performance admission and cannot satisfy W1's TapeKind
rename gate by itself.
