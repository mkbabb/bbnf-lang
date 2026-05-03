# Migration — BC Core Crate Split

Date: 2026-05-03
Status: settled. The migration cookbook for the BC.W3 core crate split. Closes surgery 34 (`audit/HARDENING-PLAN-SYNTHESIS-2026-05-03.md:74`) and F07-7 (`audit/HARDENING-PLAN-2026-05-03-07-friction-forecast.md:42`).

The bbnf-lang `crates/core/` god-crate fractures during BC.W3 into three cohesive sister crates: `bbnf-parse`, `bbnf-codegen`, `bbnf-runtime`. The umbrella `crates/core/` (still the published crate name `bbnf`) slims to a re-export shell. This document is the consumer-facing migration guide.

## §1 Crate ownership

The post-W3 workspace partitions the old `crates/core/src/` source tree along three concerns:

| Concern | Old crate | New crate | Old path | New path |
|---|---|---|---|---|
| Source compilation | `core` | `bbnf-parse` | `crates/core/src/source/` | `crates/bbnf-parse/src/source/` |
| Grammar IR construction | `core` | `bbnf-parse` | `crates/core/src/parse/` | `crates/bbnf-parse/src/parse/` |
| BBNF AST → IR lowering | `core` | `bbnf-parse` | `crates/core/src/lower/` | `crates/bbnf-parse/src/lower/` |
| Host fn shims | `core` | `bbnf-parse` | `crates/core/src/host/` | `crates/bbnf-parse/src/host/` |
| Compile pipeline | `core` | `bbnf-parse` | `crates/core/src/pipeline/` | `crates/bbnf-parse/src/pipeline/` |
| Generated parsers | `core` | `bbnf-parse` | `crates/core/src/grammar/generated/` | `crates/bbnf-parse/src/parse/generated/` |
| Codegen Rust lowerer | `core` | `bbnf-codegen` | `crates/core/src/codegen/rust/` | `crates/bbnf-codegen/src/rust/` |
| Codegen TS scaffold | `core` | `bbnf-codegen` | `crates/core/src/codegen/ts/` | `crates/bbnf-codegen/src/ts/` |
| Codegen WASM scaffold | `core` | `bbnf-codegen` | `crates/core/src/codegen/wasm/` | `crates/bbnf-codegen/src/wasm/` |
| Codegen optimiser | `core` | `bbnf-codegen` | `crates/core/src/codegen/optimiser/` | `crates/bbnf-codegen/src/optimiser/` |
| Per-grammar runtime | `core` | `bbnf-runtime` | `crates/core/src/runtime/<g>/` | `crates/bbnf-runtime/src/runtime/<g>/` |
| Path executor | `core` | `bbnf-runtime` | `crates/core/src/path/` | `crates/bbnf-runtime/src/path/` |
| Visitor trait | `core` (post-BB) | `bbnf-runtime` | `crates/core/src/visitor.rs` | `crates/bbnf-runtime/src/visitor.rs` |
| Document handle | `core` | `bbnf-runtime` | `crates/core/src/handle.rs` | `crates/bbnf-runtime/src/handle.rs` |

The `crates/path/` proc-macro crate, the `crates/path-core/` shared AST, and the `crates/path-ts/` cdylib are unaffected by the split (they were already separate per Lock 7). The umbrella `bbnf` crate (at `crates/core/`) retains `lib.rs` only as the public-facing re-export shell.

## §2 Import migration

The umbrella `bbnf` crate's `lib.rs` provides backwards-compatibility re-exports through BC.W6. Direct sub-crate imports are recommended for new code. The table below covers the most-frequent consumer-facing imports.

### §2.1 Production-API imports (most consumers)

| Pre-W3 | Post-W3 (umbrella) | Post-W3 (direct) | Notes |
|---|---|---|---|
| `use bbnf::parse::compile_grammar;` | `use bbnf::parse::compile_grammar;` (unchanged) | `use bbnf_parse::compile_grammar;` | umbrella preserves path |
| `use bbnf::runtime::json::{JsonDocument, JsonValue};` | `use bbnf::generated::json::{JsonDocument, JsonValue};` | `use bbnf_parse::generated::json::{JsonDocument, JsonValue};` | per-grammar runtime types relocated to generated modules |
| `use bbnf::runtime::Visitor;` | `use bbnf::runtime::Visitor;` (unchanged) | `use bbnf_runtime::Visitor;` | visitor trait moves to runtime |
| `use bbnf::path::{pointer, PathQuery};` | `use bbnf::path::{pointer, PathQuery};` (unchanged) | `use path::pointer; use path_core::PathQuery;` | path crates already separate per Lock 7 |
| `use bbnf::handle::DocumentHandle;` | `use bbnf::handle::DocumentHandle;` (unchanged) | `use bbnf_runtime::DocumentHandle;` | handle moves with runtime |
| `JsonParser::parse(input)` | `JsonParser::parse(input)` (unchanged) | `bbnf_parse::generated::json::JsonParser::parse(input)` | per-grammar parser typealias unchanged |

### §2.2 Codegen-API imports (xtask + advanced consumers)

| Pre-W3 | Post-W3 (umbrella) | Post-W3 (direct) | Notes |
|---|---|---|---|
| `use bbnf::backend::Emitter;` | `use bbnf::codegen::Emitter;` | `use bbnf_codegen::Emitter;` | `backend/` namespace renames to `codegen/` per BC.W3d |
| `use bbnf::backend::rust::RustEmitter;` | `use bbnf::codegen::rust::RustLowerer;` | `use bbnf_codegen::rust::RustLowerer;` | renamed (emitter → lowerer) per BC.W1 |
| `use bbnf::backend::ts::TsEmitter;` | `use bbnf::codegen::ts::TsEmitter;` | `use bbnf_codegen::ts::TsEmitter;` | unchanged on rename |
| `use bbnf::backend::wasm::WasmEmitter;` | `use bbnf::codegen::wasm::WasmEmitter;` | `use bbnf_codegen::wasm::WasmEmitter;` | unchanged on rename |
| `use bbnf::ir::TypedIRNode;` | `use bbnf::ir::TypedIRNode;` (unchanged) | `use bbnf_ir::TypedIRNode;` | bbnf-ir stays as `crates/ir/`, package name `bbnf-ir` |

### §2.3 Internal imports (within the bbnf workspace)

These are not consumer-facing; this row is for downstream crates within the bbnf workspace (e.g., `analysis`, `lsp`, `bootstrap`).

| Pre-W3 | Post-W3 | Notes |
|---|---|---|
| `use bbnf::lower::Lower;` | `use bbnf_parse::Lower;` | `lower/` migrates to bbnf-parse |
| `use bbnf::pipeline::compile;` | `use bbnf_parse::pipeline::compile;` | unchanged through namespace |
| `use bbnf::source::TopLevel;` | `use bbnf_parse::source::TopLevel;` | unchanged through namespace |
| `use bbnf::host::css_l4::parse_hex_color;` | `use bbnf_parse::host::css_l4::parse_hex_color;` | per-grammar host namespaces per surgery G05-1 |

## §3 Re-export sunset rules

Per `audit/W3-crate-dependency-dag.md:§3`, the umbrella's re-export channel partially retires at BC.W6.

| Re-export | Retains permanent? | Sunsets at | Reason |
|---|---|---|---|
| `pub use bbnf_parse::*;` | No | BC.W6 | downstream consumers migrate to direct imports |
| `pub use bbnf_codegen::*;` | No | BC.W6 | same |
| `pub use bbnf_runtime::*;` | No | BC.W6 | same |
| `pub use bbnf_runtime::Visitor;` | Yes | (permanent) | visitor is the canonical trait surface; convenience re-export warranted |
| `pub use path::pointer;` | Yes | (permanent) | path macro is consumer-frequent; convenience re-export warranted |
| `pub mod parse { pub use bbnf_parse::*; }` (namespaced) | No | BC.W6 | the umbrella does not retain namespaced re-exports past BC.W6 |
| `pub use bbnf_parse::generated::*;` | Yes | (permanent) | per-grammar generated types are stable consumer surfaces |

Migration discipline: at BC.W6, every downstream consumer is expected to migrate from `use bbnf::*;` to direct sub-crate imports for codegen / parse / runtime types. Convenience re-exports (Visitor, pointer, generated) survive in perpetuity. The migration window is the BC.W3-W6 span (~4 sub-waves).

## §4 Build-time impact

The split improves per-concern iter loops at the cost of a marginal umbrella compile overhead.

| Iter-loop | Pre-W3 wall | Post-W3 wall | Improvement |
|---|---:|---:|---|
| `cargo check -p bbnf` (umbrella) | ~22 s | ~24 s | -9% (re-export resolution overhead) |
| `cargo check -p bbnf-parse` | n/a | ~14 s | new (smaller compile graph; no codegen surface) |
| `cargo check -p bbnf-codegen` | n/a | ~12 s | new (codegen-only) |
| `cargo check -p bbnf-runtime` | n/a | ~8 s | **new (runtime-only; 65% improvement vs umbrella for runtime edits)** |
| `cargo xtask regen --check` | ~23 s | ~22 s | improved (smaller compile graph for codegen edits) |
| `cargo nextest run --workspace` | ~50 s | ~50 s | unchanged |
| `cargo nextest run -p bbnf-runtime` | n/a | ~12 s | new (runtime-only test surface) |

The runtime-only iter loop is the biggest win for path / visitor / value-type edits. The codegen-only iter loop benefits from skipping the parse + IR compile surface for emitter-only changes.

## §5 Troubleshooting

### §5.1 "Cannot resolve `bbnf::backend::*`"

Cause: `backend/` namespace renamed to `codegen/` at BC.W3d.

Fix: rewrite `bbnf::backend::*` to `bbnf::codegen::*` (or direct `bbnf_codegen::*`). The umbrella does not re-export `backend` post-W3d.

### §5.2 "Cannot resolve `bbnf::runtime::json::JsonValue`"

Cause: per-grammar runtime types relocated to generated modules per `audit/RESTART-SKETCH-2026-05-03.md:444-458` and `feedback_doc_alongside_code`.

Fix: rewrite `bbnf::runtime::json::JsonValue` to `bbnf::generated::json::JsonValue` (or direct `bbnf_parse::generated::json::JsonValue`). The umbrella's `bbnf::runtime::<g>` paths sunset at BC.W6.

### §5.3 "Cargo cannot resolve `parse-that/rust/regex` after rename"

Cause: BC.W5b renames `parse-that/rust/regex` → `parse-that/rust/bbnf-regex` per `audit/W5-bbnf-regex-endpoint-decision.md`.

Fix: update `.cargo/config.toml` `[patch.crates-io]` entry from `parse-that/rust/regex` to `parse-that/rust/bbnf-regex`. The migration is one-line; bbnf-lang's own `.cargo/config.toml` is updated at W5b.

### §5.4 "Tests reference `crate::runtime::*` paths that no longer resolve"

Cause: per-crate tests migrated with their owning sub-crate; tests at the old `crates/core/tests/` may reference paths that now live under `crates/bbnf-runtime/`, `crates/bbnf-parse/`, or `crates/bbnf-codegen/`.

Fix: relocate tests to their owning sub-crate. Cross-crate integration tests live at `crates/core/tests/` (the umbrella) and use `bbnf::*` imports through the umbrella's re-exports. See §3 re-export sunset rules for which paths persist.

### §5.5 "`cargo check -p bbnf-parse` fails with `error: cannot find type IrNode`"

Cause: `bbnf-parse` does NOT depend on `bbnf-codegen`; the typed IR alphabet lives in `bbnf-ir` (`crates/ir/`); `bbnf-parse` consumes via `use bbnf_ir::types::grammar::IrNode`.

Fix: ensure `bbnf-parse/Cargo.toml` has `bbnf-ir = { workspace = true }` and import via `use bbnf_ir::*`. The dependency arrow `bbnf-parse → bbnf-ir` is part of the W3 contract.

### §5.6 "Workspace metadata `[workspace.metadata.bbnf-strategy]` not found"

Cause: per BA.W1 (handled in BA tranche), workspace metadata schema is the new strategy resolver entry. Not BC scope.

Fix: see `docs/tranches/BA/audit/W1-workspace-metadata-schema.md` for the schema; this is BA scope, not BC. BC inherits the metadata table without modification.

### §5.7 "I want to publish a crate that depends on bbnf-codegen"

Cause: `bbnf-codegen` is workspace-internal post-BC; future publication candidacy is post-BD. Per `audit/W3-crate-dependency-dag.md:bbnf-ir`, the same posture applies to `bbnf-ir`.

Fix: vendor bbnf-codegen as a path-dep in your downstream workspace; or wait for the post-BD publication wave. The umbrella `bbnf` crate is the only published surface in the BC era.

### §5.8 "`cargo tree -p bbnf-parse` shows `bbnf-codegen`"

Cause: dependency-arrow violation per W3-G3; this should never happen.

Fix: this is a regression. `bbnf-parse` MUST NOT depend on `bbnf-codegen` per `audit/W3-crate-dependency-dag.md:§5`. File a bug; the W3 closer-gate explicitly verifies this absence.

## §6 Cross-references

| Reference | Description |
|---|---|
| `audit/MODULES-2026-05-03.md:1158-1167` | The crate split source-of-truth |
| `audit/W3-crate-dependency-dag.md` | The dependency arrow specification |
| `audit/W3-generated-output-relocation.md` | Generated output path relocation |
| `audit/W5-bbnf-regex-endpoint-decision.md` | bbnf-regex rename decision |
| `audit/W5-parse-that-disposition.md` | parse-that publication posture |
| `audit/CENSUS-2026-05-03.md:103-109` | Per-grammar host namespace discipline |
| `feedback_doc_alongside_code` | Per-grammar runtime types relocate to generated modules |
| `feedback_no_workarounds_arch` | The split is mandatory architectural transposition |

## §7 Closing posture

The BC core split is structural. The behavioural surface is unchanged. The umbrella crate retains backwards compatibility through BC.W6 via re-exports; new code targets direct sub-crate imports. The runtime-only iter loop, the codegen-only iter loop, and the parse-only iter loop each shrink the compile graph for per-concern edits. Lock 13 honoured at the crate level. The migration window is BC.W3 → BC.W6.
