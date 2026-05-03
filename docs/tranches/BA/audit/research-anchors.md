# BA Research Anchors

Date: 2026-05-03
Scope: BA-tranche deep-research input. Primary sources only. ≤ 200 lines.

## §1 — 9-directory layered re-org references

### sonic-rs `src/` layout

Per `audit/SOTA-2026-05-03.md:28` (`/src/lib.rs`, `/src/lazyvalue/`, `/src/value/`, `/src/pointer/`, `/src/parser.rs`). Sonic-rs separates the parse driver (`parser.rs`), the materialised typed value (`value/`), the lazy-borrow surface (`lazyvalue/`), and the path API (`pointer/`) into sibling concerns. Each sibling carries a uniform surface API: `value/` exports `Value`, `value/mod.rs` re-exports; `lazyvalue/` exports `LazyValue` + `OwnedLazyValue`; `pointer/` exports `JsonPointer` + the `pointer!` macro.

The siblings are cohesive (each holds one concern), the directory is shallow (no nested `utils/` / `helpers/` god directories), and the public surface is a flat re-export from `lib.rs`. This is the canon BA.W0's 9-directory layered re-org follows.

### lightning-css `src/` layout

Per `audit/SOTA-2026-05-03.md:97` (`/src/visitor.rs`, `/src/properties/mod.rs`, `/src/values/`, `/src/parser.rs`, `/src/macros.rs`, `/src/rules/`, `/src/stylesheet.rs`). lightningcss's structure splits at the AT-rule / property / value / declaration / selector / visitor axes. `src/properties/` has 60+ per-property files; `src/rules/` has 30+ per-AT-rule files; `src/values/` has 20+ per-value-type files. The pattern is **per-record file**: each record sum gets its own file; the parent module is a re-export hub.

This is the apotheosis of `feedback_no_god_modules`: every record carries its own file, no single file >500 LOC, the directory structure mirrors the grammar's record taxonomy. BA.W2's god-module splits (23 → 0) follows this discipline.

### Sibling-API uniformity

Both sonic-rs and lightning-css enforce **sibling-API uniformity**: every per-record sub-module exports the same shape (e.g. `Parse`, `ToCss`, `Visit` for lightning-css; `JsonValueTrait` for sonic-rs). BA.W2's split discipline (per `audit/MODULES-2026-05-03.md` per-file recommendations) enforces the same: every shape emitter sub-module exports `emit` + `register`; every recogniser sub-module exports `mine` + `findings`.

## §2 — Cargo workspace metadata patterns

### `cargo_metadata` crate

`cargo_metadata` (https://crates.io/crates/cargo_metadata) parses `cargo metadata --format-version 1` JSON output. The `Metadata::workspace_metadata` field returns `serde_json::Value`; consumers `serde_json::from_value::<MyMetadataSchema>` to parse `[workspace.metadata.<namespace>]` tables. The xtask consumer at `xtask/src/` reads workspace metadata via this pattern. The schema BA.W1 commits to (gap F) is consumed via this surface.

### cargo-deny.toml schema

`cargo-deny` (https://github.com/EmbarkStudios/cargo-deny) reads `deny.toml` at workspace root with sections `[graph]`, `[advisories]`, `[licenses]`, `[bans]`, `[sources]`. Each section admits typed fields with explicit `default` values and validation rules; unknown keys error. The pattern: declarative TOML at workspace root, schema enforced by the consumer crate. BA.W1's `[workspace.metadata.bbnf-strategy]` follows the cargo-deny precedent: validated schema, default values for optional fields, hard error on unknown keys.

### cargo-msrv.toml schema

`cargo-msrv` (https://github.com/foresterre/cargo-msrv) admits `[package.metadata.msrv]` with `version`, `command`, `target`. Per-package metadata table with mandatory fields. BA.W1's per-grammar `[workspace.metadata.bbnf-strategy.grammars.<ident>]` is a per-grammar variant of this pattern.

## §3 — Inline test migration

Per `audit/CENSUS-2026-05-03.md:381-399`, eight inline `#[cfg(test)]` violations:

| Path:line | Migration target |
|---|---|
| `crates/core/src/path/cursor.rs:313` | `crates/core/tests/path_cursor.rs` |
| `crates/core/src/path/executor.rs:65` | `crates/core/tests/path_executor.rs` |
| `crates/core/src/path/schema.rs:130` | `crates/core/tests/path_schema.rs` |
| `crates/core/src/runtime/google_sheets/parse_with.rs:83` | `crates/core/tests/parse_with_google_sheets.rs` (extant) |
| `crates/core/src/runtime/css_l4/parse_with.rs:84` | `crates/core/tests/parse_with_css_l4.rs` (extant) |
| `crates/core/src/runtime/bbnf/parse_with.rs:99` | `crates/core/tests/parse_with_bbnf.rs` (extant) |
| `crates/core/src/runtime/json/parse_with.rs:105` | `crates/core/tests/parse_with_json.rs` (extant) |
| `crates/core/src/backend/rust/analysis/inline/mod.rs:37` | `crates/core/tests/inline_analysis.rs` |

Per `feedback_no_inline_tests` ZERO inline blocks survive BA close. BA.W0.M5 closes all eight.

## §4 — Era V failure-mode anatomy

Per `docs/tranches/meta-audit/archaeology/era-V-dta-psi-rut.md:7-10`: "Era V's signature failure mode is **substrate-first-consumer-later**: every tranche ships the compile-time emission of constants, tables, and shape dictionaries; no tranche fully activates the runtime consumer that reads them."

The substrate-without-consumer signatures:

- A tranche emits a new IR field (e.g. `bbnf_shape_templates: Vec<BbnfShapeTemplate>`) and a sibling tranche eventually consumes it.
- A tranche introduces a trait method (e.g. `EmitStrategy::for_grammar`) and a sibling tranche specialises it.
- A tranche produces a sidecar artefact (e.g. `<grammar>.registry.json`) and a sibling tranche reads it.

The mitigation invariant: every wave's substrate is consumed in the SAME WAVE (the producing wave is also the consuming wave) or in the IMMEDIATE NEXT WAVE (the producing wave's §6 cross-references names the consumer wave). BA satisfies this invariant per the per-wave §6 "Cross-references" tables; the synthesis pass at BA close (W6.M1 lock cross-reference verification) audits cell-by-cell.

## §5 — Direct-projection emit anchors (sonic-rs JSON)

Per `audit/SOTA-2026-05-03.md:30`: "sonic-rs directly parses the JSON into a Rust struct, and there are no temporary data structures." Per `audit/SOTA-2026-05-03.md:212`: "Direct-to-struct (no tape) | sonic-rs | One pass; minimal allocations; no skip benefit."

The sonic-rs direct-projection emit shape (per `audit/RESTART-SKETCH-2026-05-03.md:512-543` worked example): `parse_value(input, &mut p, ...) -> Result<JsonValue<'p>, ParseErr>` with byte-disjoint Alt emitting `match first { b'{' => parse_object(...), b'[' => parse_array(...), b'"' => parse_string(...), b'-' | b'0'..=b'9' => parse_number(...), b't' | b'f' => parse_bool(...), b'n' => parse_null(...) }` — no `OpenFrame` push, no `checkpoint`, no `Vec::clone`. The recursion stack IS the open-compound stack.

This is the canonical shape BA.W5's direct-to-struct emit produces; the per-construct emission shapes are codified at `docs/tranches/BA/audit/W5-generated-parser-shape.md`.

## §6 — Path-crate triplet references

Lock 7 names three crate names: `path`, `path-core`, `path-ts`. The proc-macro-cannot-be-path-dep limitation means `path` (proc-macro) and `path-ts` (cdylib) cannot path-dep on each other; both path-dep on `path-core` (pure rlib). Per `audit/CENSUS-2026-05-03.md:263`, the merger eliminates ~500 LOC of mirrored lex/lower/validate logic between `bbnf-path/src/path_macro.rs` (639 LOC) and `bbnf-path-ts/src/compile.rs` (474 LOC).

## §7 — Workspace metadata current state

Per the existing `Cargo.toml:18` table `[workspace.metadata.bbnf]` (grammars list with `ident`, `path`, `features`) and `:45` table `[workspace.metadata.bbnf-strategy]` (grammars list with `idents`, `rust_builder_path`, `rust_document_path`). The current schema is structurally close to the Phase-4 target; BA.W1's surgery is to remove the hardcoded `idents` from `[workspace.metadata.bbnf-strategy]` (the grammar parser-struct names) and add the recogniser plugin schema fields (`name`, `crate`, `entrypoint`, `output_kind`) plus host-fn fields.

## §8 — Lightning-css visitor pattern

Per `audit/SOTA-2026-05-03.md:103-118`, lightningcss's `Visitor` trait carries `visit_types() -> VisitTypes` (bitflags) plus per-record `visit_<Name>(&mut self, &mut <Record>)` methods. The `CHILD_TYPES` bitmask is procedurally derived; transforms only walk relevant subtrees. BB.W5's Visitor surface inherits this pattern; BA does not produce visitor scaffolding, only the IR contract precursor.

## §9 — Generated-parser-shape primary sources

- **sonic-rs `parser.rs`**: `crates/sonic-rs/src/parser.rs` shows the byte-disjoint Alt direct-match shape; no checkpoint on disjoint cases.
- **simdjson On-Demand `document.h`**: `simdjson/include/simdjson/dom/`/On-Demand traversal API; lazy iterator semantics.
- **chumsky `parser.rs`**: typed combinators with `Parser<I, O, E>`; `O` (output type) is the typed-emit target.
- **lalrpop generated outputs**: per-rule `parse_<rule>(input) -> Result<<rule_output>, lalrpop_util::ParseError>` shape; consumed by BA.W5's per-rule emit pattern.
- **pest `derive` output**: `Pairs<Rule>` iterator over pre-recorded `QueueableToken`s; rejected (untyped) but the `Rule::*` enum naming pattern informs MapExpr emission.

## §10 — Path:line citation discipline (Operational Rule re-emphasis)

Every concrete claim in BA waves carries `path:line` per Voice Lock §V3. The audit artefacts at `docs/tranches/BA/audit/W*-*.md` carry the same discipline. The synthesis pass verifies citations resolve (Read after surgery).
