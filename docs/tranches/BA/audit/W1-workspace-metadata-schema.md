# BA.W1 Workspace Metadata TOML Schema

Date: 2026-05-03
Surface: `Cargo.toml`'s `[workspace.metadata.bbnf-strategy]` table.
Consumer: `crates/ir/src/registry/strategy.rs` (post-W1 metadata-driven resolver); xtask regen.

## §1 — Schema (verbatim TOML form)

```toml
[workspace.metadata.bbnf-strategy]
# Top-level fields. Per-grammar entries live under `grammars.<ident>`.
schema_version = 1                                     # required, integer
default_pratt_eligibility = "auto"                     # optional, default "auto"
default_simd_eligibility  = "auto"                     # optional, default "auto"

# Per-grammar entries. <ident> is the grammar's canonical lowercase
# identifier (e.g. "json", "css_l4", "bbnf"). Adding a new grammar
# requires one new sub-table; the IR crate consults this table at
# xtask-regen time and never knows the grammar name at compile time.
[workspace.metadata.bbnf-strategy.grammars.<ident>]
source_path        = "grammar/<g>/<g>.bbnf"            # required, workspace-relative
bootstrap          = false                             # optional, default false (true only for "bbnf")
output_dir         = "crates/core/src/grammar/generated" # optional, default "crates/core/src/grammar/generated"
strategy           = "open_frame" | "direct_to_struct" # required; BA.W5 admits "direct_to_struct" for "json" only
audit_label        = "<ident>"                         # optional, default = <ident>
rust_builder_path  = "crate::runtime::<g>::<G>StructBuilder" # required for strategy = "open_frame"
rust_document_path = "crate::runtime::<g>::<G>Document" # required for strategy = "open_frame"
pratt_eligibility  = "auto" | "force" | "skip"         # optional, default = top-level default_pratt_eligibility
simd_eligibility   = "auto" | "force" | "skip"         # optional, default = top-level default_simd_eligibility

# Recogniser plugins. Each entry declares a structural-shape miner
# the IR pipeline should attend to for this grammar. The IR crate
# loads recognisers by registry entry; no `match grammar { ... }` arm
# survives in `bbnf-ir`.
[[workspace.metadata.bbnf-strategy.grammars.<ident>.recognizers]]
name        = "<miner_ident>"                          # required, e.g. "big_comment_pattern_miner"
crate       = "bbnf-ir"                                # required, the crate the miner lives in
entrypoint  = "passes::recognizers::<miner_path>"      # required, the function path
output_kind = "<finding_kind>"                         # required, e.g. "ShapeTemplate", "DelimScanConfig"

# Host functions. Each entry declares a host-fn the grammar's MapExpr
# annotations resolve to at codegen time. Per-grammar host fns live
# under `crates/core/src/grammar/host/<g>/` (per gap G05-9 surgery).
[[workspace.metadata.bbnf-strategy.grammars.<ident>.host_fns]]
name = "<fn_ident>"                                    # required, e.g. "parse_hex_color"
crate = "bbnf"                                         # required, the crate the fn lives in
path  = "grammar::host::<g>::<fn_ident>"               # required, the resolved Rust path
```

## §2 — Validation rules

| Rule | Enforced by |
|---|---|
| `schema_version` ≥ 1 | xtask regen pre-flight; halts on missing field |
| `<ident>` matches regex `^[a-z][a-z0-9_]*$` | xtask validation pass |
| `strategy` ∈ {`open_frame`, `direct_to_struct`} | xtask validation; unknown values halt |
| `strategy = "direct_to_struct"` requires `<ident> = "json"` (BA close); other grammars admit only `"open_frame"` until BB.W1a/W1b/W1c/W2 | xtask validation per BA-receiving-wave consultation |
| `bootstrap = true` admits only `<ident> = "bbnf"` | xtask validation |
| `recognizers[].name` uniquely keyed within `<ident>`'s table | xtask validation |
| `host_fns[].name` uniquely keyed within `<ident>`'s table | xtask validation |
| `recognizers[].entrypoint` resolves at xtask compile time (function exists) | xtask compile-time validation |
| `host_fns[].path` resolves at codegen time (path exists in target backend) | codegen validation |
| Unknown keys in `[workspace.metadata.bbnf-strategy.grammars.<ident>]` error | TOML parse with `deny_unknown_fields = true` |

## §3 — Optional vs required cross-reference

| Field | Required | Default |
|---|:---:|---|
| `schema_version` | Y | — |
| `default_pratt_eligibility` | N | `"auto"` |
| `default_simd_eligibility` | N | `"auto"` |
| `grammars.<ident>.source_path` | Y | — |
| `grammars.<ident>.bootstrap` | N | `false` |
| `grammars.<ident>.output_dir` | N | `"crates/core/src/grammar/generated"` |
| `grammars.<ident>.strategy` | Y | — |
| `grammars.<ident>.audit_label` | N | `<ident>` |
| `grammars.<ident>.rust_builder_path` | Y (when `strategy = "open_frame"`) | — |
| `grammars.<ident>.rust_document_path` | Y (when `strategy = "open_frame"`) | — |
| `grammars.<ident>.pratt_eligibility` | N | top-level default |
| `grammars.<ident>.simd_eligibility` | N | top-level default |
| `grammars.<ident>.recognizers[].name` | Y | — |
| `grammars.<ident>.recognizers[].crate` | Y | — |
| `grammars.<ident>.recognizers[].entrypoint` | Y | — |
| `grammars.<ident>.recognizers[].output_kind` | Y | — |
| `grammars.<ident>.host_fns[].name` | Y | — |
| `grammars.<ident>.host_fns[].crate` | Y | — |
| `grammars.<ident>.host_fns[].path` | Y | — |

## §4 — Recogniser plugin schema (Surgery #16 surface)

Per surgery #16 (`docs/PHASE-4-DIRECTIVE-2026-05-03.md` §2 BA list), the recogniser plugin schema admits four fields per entry: `name`, `crate`, `entrypoint`, `output_kind`. The fields:

- `name`: the miner's canonical identifier (e.g. `big_comment_pattern_miner`, `dimension_unit_miner`); used as the lookup key in `recogniser_outputs: HashMap<String, Vec<RecognizerFinding>>`.
- `crate`: the Rust crate where the miner lives; defaults to `bbnf-ir` for in-tree miners; future external miners declare other crates.
- `entrypoint`: the Rust path of the miner function; e.g. `passes::recognizers::big_comment::mine`. The IR crate's recogniser orchestrator at `crates/ir/src/passes/recognizers/mod.rs` invokes the path via dynamic resolution at xtask-regen time.
- `output_kind`: the typed-finding kind the miner produces; e.g. `ShapeTemplate`, `DelimScanConfig`. Constrains the deserialisation surface; the orchestrator routes findings to the consumer that expects that kind.

The schema generalises the BA-pre-restart `bbnf_shape_templates: Vec<BbnfShapeTemplate>` field on `GrammarProfile` (per `audit/CENSUS-2026-05-03.md:118` DELETE) — that field's findings now arrive via `recogniser_outputs.get("big_comment_pattern_miner")` keyed by string.

## §5 — Host-fn schema (Surgery #15 surface)

Per surgery #15, host fns relocate to per-grammar host namespaces: `grammar/host/css_l4.rs`, `grammar/host/google_sheets.rs`, etc. — no `host/<grammar>.rs` generic-root pattern. The schema's `path` field admits the resolved Rust path; codegen splices `crate::grammar::host::css_l4::parse_hex_color` (CSS L4) or `crate::grammar::host::google_sheets::parse_currency_format` (Sheets) directly into emitted parsers.

## §6 — Migration plan from current `[workspace.metadata.bbnf-strategy]`

The current schema (per `Cargo.toml:45-56`) admits 9 grammar entries with three fields: `idents` (parser-struct names), `rust_builder_path`, `rust_document_path`. The W1 surgery:

| Step | Action | Validation |
|---|---|---|
| 1 | Add `schema_version = 1` to top-level | TOML parse |
| 2 | Convert each grammar's flat entry to `[workspace.metadata.bbnf-strategy.grammars.<ident>]` sub-table | xtask validation |
| 3 | Drop the `idents` array (parser-struct names hardcoded the IR's strategy resolver per CENSUS:115); replace with `audit_label` (per-grammar canonical label) | grep zero `JsonParser` / `CssL4Parser` etc. in `crates/ir/src/registry/strategy.rs` |
| 4 | Add `strategy` field per grammar; JSON gets `"direct_to_struct"` (BA.W5); others get `"open_frame"` (deferred to BB.W1a/W1b/W1c/W2) | xtask validation |
| 5 | Add `recognizers` array per grammar; populate from BA.W1.M4 generalised registry (BBNF gets `big_comment`, `mapped_factor`; CSS L4 gets `color_function`, `dimension_unit`; etc.) | post-W1.M4 verification |
| 6 | Add `host_fns` array per grammar; populate from BA.W0.M1 host-relocation (CSS L4 gets `parse_hex_color`); others empty until BB.W1a/W1b/W1c | post-W0.M1 verification |
| 7 | Update `crates/ir/src/registry/strategy.rs` to consume the new schema via `cargo_metadata::MetadataCommand::new()`; cache via `LazyLock<Vec<StrategyEntry>>` | `cargo nextest run -p bbnf-ir` |
| 8 | Delete the static `GRAMMARS` array at `crates/ir/src/registry/strategy.rs:130-185` per CENSUS:115 | `rg -n 'static.*GRAMMARS' crates/ir/src/registry/` returns zero |

## §7 — Breaking-change notice strategy

Per `feedback_no_backward_compat`, the migration is a full rewrite at BA.W1 close. No transitional alias survives. The downstream impact:

- **xtask regen consumers**: read the new schema; the consumer call `cargo_metadata::MetadataCommand::new().exec()?.workspace_metadata` returns the typed `BbnfStrategy` struct.
- **Consumer crates** (analysis, lsp): consume strategy entries via `bbnf_ir::registry::strategy::load_strategy_entries() -> &'static [StrategyEntry]`; the public surface is preserved across the migration.
- **Test fixtures**: `crates/ir/tests/payload_coverage_audit.rs` consumes `GrammarAuditTag::Custom("json")` (post-W1.M2 collapse from named arms); the rewrite is mechanical.

The break is contained to the IR crate's strategy resolver and the workspace metadata table; no public bbnf API surface changes.

## §8 — Closer disposition

The schema is the single source of truth for grammar registration post-BA.W1. Adding a new grammar is one TOML entry plus one `grammar/<ident>/<ident>.bbnf` source file; no `bbnf-ir` source code change is required. The schema is consumed at xtask-regen time and at codegen time (host-fn path resolution); the IR crate's grammar-agnostic surface is the contract.
