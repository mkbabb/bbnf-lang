# SK-V7 W0b R1: report schema research

Scope: read-only inspection of the `bbnf-bench` report and gate paths that
generate `skinny/RESULTS.md`. No parser/runtime changes are in scope.

## Source of Truth

`restart/prompts/pass-contracts/PASS-ALPHA.md` §4.3 is the schema authority. It
names 26 fields; `restart/skinny/tranches/sk-v7/SPEC.md` says 24 columns, but
W0b should key correctness to the named fields, not the stale count.

## Owner Files

| File | Role |
|---|---|
| `skinny/crates/bbnf-bench/src/report.rs` | `Report`, row structs, markdown rendering, and report tests. Primary schema-v3 insertion point. |
| `skinny/crates/bbnf-bench/src/bin/gate.rs` | Reads Criterion artefacts, classifies rows, pushes report rows, writes `RESULTS.md`, and exits with gate verdict. |
| `skinny/crates/bbnf-bench/src/metadata.rs` | Criterion-side `metadata.toml` schema. Still `SCHEMA_VERSION = "2"` at research time. |
| `skinny/crates/bbnf-bench/benches/json_parity.rs` | Emits benchmark names and metadata rows consumed by `gate.rs`. |
| `skinny/xtask/src/main.rs` | `gate-json` is a wrapper around `cargo run -p bbnf-bench --bin gate`; schema rejection should live in `bbnf-bench` unless xtask is expanded. |
| `skinny/RESULTS.md` | Generated output. It should remain generated, not hand-maintained. |

## Current Generation Path

1. `json_parity.rs` writes Criterion metadata under
   `target/criterion/json_{corpus}/{bench}/metadata.toml`.
2. `gate.rs` reads slope estimates into `Estimates`, classifies parse and
   workload outcomes, then calls `report.push_row` and
   `report.push_workload_row`.
3. `report.rs` hard-codes markdown headers and row provenance strings.
4. `gate.rs` writes `skinny/RESULTS.md` and exits non-zero for the worst
   current verdict.

## Current Schema Gaps

Parse rows currently omit `Workload`, use one `sonic-rs Mbps` column, and lack
the `sonic-rs strict`, `sonic-rs lossy`, `simdjson DOM`, `simdjson On Demand`,
`yyjson`, `asmjson`, `RapidJSON`, `serde_json`, delta, `Hot leaf`, and
`Signal` fields required by PASS-ALPHA §4.3.

Workload rows already have `Workload`, `serde_json`, and `Signal`, but still
lack the full comparator matrix and delta/hot-leaf columns.

The `Masking Probes` section should stay separate unless a later wave promotes
probes into `memory` or `cycles_per_byte` workload rows.

## Recommended Shape

- Make `report.rs` schema-first with one telemetry row type that carries the
  PASS-ALPHA columns verbatim. Use `parse_only` for parse rows.
- Move strictness/provenance data into `RowMetadata` instead of rendering it
  from prose constants.
- Bump metadata to schema v3 so old Criterion artefacts become invalid and a
  fresh bench is required.
- Add parse `serde_json` estimates to the gate; do not relabel Rust
  `simd-json` as C++ `simdjson DOM`.
- Put schema rejection in the gate binary via `Report::validate_schema_v3()`;
  `xtask gate-json` can remain a thin wrapper.

## Risks

- Cargo feature unification: a lossy `sonic-rs` dependency alias would poison
  the strict package build.
- Comparator collision: Rust `simd-json borrowed/owned` is not C++ simdjson
  DOM/On Demand.
- `Delta vs SK-V6` needs an explicit baseline source.
- `Hot leaf` is not Criterion data; W0b may render `unprofiled in W0b` only if
  the signal refuses to prescribe kernels from it.
- Schema validation failure must remain distinguishable as `JSchemaFail`, not
  ordinary `N-direct / NoGo`.
