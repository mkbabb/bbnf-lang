# Pass A — Agent A.5 — Replacement Design

Date: 2026-05-03
Lens: for every file Agents A.2 / A.3 mark ABROGATE-REPLACE, design the new
facility. Plus: identify brand-new items the absence of which is felt.

---

## §1 — Replacement table (abrogated → new facility)

| # | Abrogated item | New facility | Justification | Located at | Implementation sketch |
|---|---|---|---|---|---|
| 1 | `crates/core/src/css_types.rs` (66 LOC, `pub fn parse_hex_color`) | per-grammar declaration crate `crates/bbnf-grammar-css-l4/` exposing `host::parse_hex_color` | Lock 14: grammar-specific code lives in per-grammar declaration crates only. Generic crates carry zero grammar-named host fns. | `crates/bbnf-grammar-css-l4/src/host.rs` | new crate manifest declares the host fn surface; CSS L4 grammar's `-> parse_hex_color(...)` map reference rewrites to `bbnf_grammar_css_l4::host::parse_hex_color`; codegen emits absolute paths into the per-grammar host module. |
| 2 | `crates/ir/src/passes/recognizers/shape_dict_bbnf.rs` (192 LOC, BBNF-named shape miner) | metadata-driven structural-shape miner registry | Lock 14: zero grammar-named modules in IR. The mining of BBNF's `big_comment` and `mapped_factor` shapes is structurally generic — it's "find rule X with body shape Y"; the BBNF-specific knowledge belongs in workspace metadata, not IR-source. | `bbnf-passes/src/recognizers/structural_shape/` (mechanism); per-grammar metadata under `[workspace.metadata.bbnf.grammars.<g>.shape-templates]` (data) | mechanism: `mine_structural_shapes(ir: &GrammarIR, templates: &[ShapeTemplateSpec]) -> Vec<DetectedShape>`; `ShapeTemplateSpec` is a small DSL ({rule_name pattern, body_pattern, payload_kind}) parsed from workspace metadata. The BBNF templates (big_comment, mapped_factor empty branch) become two metadata rows. Adding a per-grammar shape template is metadata + zero source code. |
| 3 | `crates/ir/src/passes/audit/payload_coverage.rs` (`enum GrammarAuditTag::{Json, CssL4, Sheets, Bbnf, Custom(&'static str)}` at L67-77) | unified `GrammarAuditTag(&'static str)` newtype | Lock 14: zero grammar-named arms. The named arms are pure ergonomic sugar; the underlying machinery already supports `Custom(&'static str)`. | `bbnf-passes/src/audit/payload_coverage.rs` (post-rename) | `pub struct GrammarAuditTag(pub &'static str);` newtype; key() method returns the string directly; downstream JSON output keys the same way. The four named-arm constants `JSON`, `CSS_L4`, `SHEETS`, `BBNF` are exported as `pub const` for ergonomics if desired (still no source-arm logic). |
| 4 | `crates/ir/src/registry/strategy.rs:130-185` (`PRODUCTION_MANIFEST_TABLE` Rust-source mirror of workspace metadata) | xtask-codegen-time read of `[workspace.metadata.bbnf-strategy]` directly into the IR's `EmitStrategy` resolution | Lock 14: zero hardcoded grammar-name table in IR. The mirror exists today because xtask reads workspace metadata and the IR has its own table. Single source of truth. | xtask reads metadata; IR's `EmitStrategy::for_grammar(grammar_ident, &registry, &strategy_table)` consumes a passed-in table | `pub struct StrategyTable(BTreeMap<&'static str, SubstrateBinding>);` constructed by xtask at codegen time from workspace metadata; passed through to `for_grammar`. The IR's library crate carries no static manifest. |
| 5 | `crates/ir/src/passes/profile.rs:26, 108` (`bbnf_shape_templates: Vec<BbnfShapeTemplate>` field on `GrammarProfile`) | the field is removed; structural-shape templates land via the generalised recogniser pipeline (replacement #2) | Lock 14: no grammar-named field on universal struct. | `bbnf-passes/src/profile.rs` (post-rename) | replacement #2's `mine_structural_shapes` populates a `Vec<DetectedShape>` field on `GrammarProfile` (renamed `structural_shapes`); BBNF's templates become two of those shapes. |
| 6 | `crates/bbnf-path/src/registry.rs:132-135` (`match grammar { "json" => ..., "css_l4" => ..., "google_sheets" => ..., "bbnf" => ... }`) | workspace-metadata-driven registry resolver | Lock 14: zero grammar-string-match in proc-macro. | `path-core/src/registry.rs` (post Proposal 3) | `pub struct RegistryDescriptor { ident: &'static str, layout_path: &'static str, ... }`; the proc-macro consumes a slice `&[RegistryDescriptor]` produced by xtask from workspace metadata. The metadata table is the single source of truth. |
| 7 | `crates/bbnf-path/src/registry.rs` (synthetic fixture `StructRegistry` lookups; 201 LOC) + `crates/bbnf-path-ts/src/fixture.rs` (mirror; 248 LOC) | per-grammar emitted `pub const REGISTRY: Layout` at xtask-regen time | Lock 14 + DRY: one source of truth; both proc-macro and cdylib consume the production const. | `bbnf-codegen/src/generated/<g>/registry.rs` | xtask emits one `registry.rs` per grammar carrying `pub const REGISTRY: bbnf_ir::Layout = ...` (the layout is data; serialisable); both `path` (proc-macro) and `path-ts` (cdylib) path-dep on `bbnf-ir` and consume the per-grammar `REGISTRY`. The synthetic fixture surface retires. |
| 8 | `crates/core/src/path/markers.rs` (per-grammar ZSTs hardcoded) | per-grammar declaration-crate-emitted markers | Lock 14: per-grammar types live in per-grammar declaration crates. | `bbnf-codegen/src/generated/<g>/marker.rs` (or per-grammar declaration crate) | xtask emits `pub struct <G>;` per grammar; the path crate carries only the `GrammarMarker` trait. Adding a grammar adds one type; no edit in the path crate. |
| 9 | `crates/core/src/runtime/path.rs` (legacy borrowed alphabet, 163 LOC, duplicate of `path/ir.rs`) | (deleted; runtime documents adopt the typed alphabet from `path-core`) | KISS + DRY: one alphabet | (deleted) | runtime documents' `*PathQuery` trait consumes `path_core::ir::Path<'a>` directly; the manual lowering in `runtime/<g>/parse_with.rs` retires. |
| 10 | `crates/core/src/runtime/<g>/parse_with.rs` legacy lowering (4 files, ~480 LOC) | typed-path-direct path threading | KISS: the typed alphabet IS the runtime alphabet | (post-restructure) | `parse_with(input, path: &TypedPath<G, T>)` consumes typed segments directly; the manual lower-to-legacy step disappears. |
| 11 | `crates/core/src/grammar/generated/mod.rs:35` `pub use bbnf::*` aggregator | namespaced access for every grammar uniformly | Lock 14 + system-cohesion (asymmetry between BBNF and other grammars) | `crates/core/src/grammar/generated/mod.rs` | drop the `pub use bbnf::*` line; consumers reach BBNF via `bbnf::grammar::generated::bbnf::BbnfBootstrap` like every other grammar. |
| 12 | `crates/core/src/lower/value_expr/simple_kinds.rs:185` "Defensive fallback" | structural exhaustiveness | Lock § no-workarounds | (in-place edit) | replace the defensive fallback arm with `unreachable!("simple_kinds: <BbnfValue variant> reached descend; upstream guarantees …")` if the case is provably unreachable, or fix the upstream classifier to surface the missing variant. |
| 13 | `crates/core/src/grammar/host.rs:387` wildcard `@debug` strip-prefix fallback | explicit keyword enumeration with FAIL-EXPLICIT on unknown | Lock § no-workarounds + no-silent-epsilon | (in-place edit) | match every recognised debug-keyword; unknown-keyword arm panics with a structured diagnostic citing the offending source span. |
| 14 | `crates/core/src/pipeline.rs` + `crates/core/src/pipeline/` flat-file + sibling-directory pair | `pipeline/mod.rs` with directory-structure | `feedback_directory_modules` | `crates/core/src/pipeline/mod.rs` | merge `pipeline.rs` content into `pipeline/mod.rs`; delete `pipeline.rs`. The `pipeline/` directory becomes a clean directory module. |
| 15 | `crates/bbnf-path/src/path_macro.rs` (639 LOC god module) | phase-split sub-modules | Lock 13 | `path-core/src/{lex,lower,validate,emit}.rs` (per Proposal 3) | proc-macro entry consumes phase-split sub-modules; each phase ≤ 200 LOC. |
| 16 | `crates/ir/src/passes/types/` (the directory; Lock 2 retired vocabulary) | `bbnf-passes/src/layout/` | Lock 2 | `bbnf-passes/src/layout/{mod,solver,projection,lifetime,registry_glue}.rs` | rename + split. The 786-LOC `mod.rs` god module fractures by concern. |
| 17 | `crates/ir/src/types/type_desc.rs` (`TypeDesc` type — Lock 2 retired) | `Layout` representation | Lock 2 | `bbnf-ir/src/types/layout.rs` | rename type + every consumer; `TypeDesc` becomes either a private `Layout::Atom` variant or disappears entirely. |
| 18 | `crates/ir/src/registry/struct.rs` (`StructLayout`, `StructRegistry` — Lock 2 retired) | `Layout`, `LayoutRegistry` | Lock 2 | `bbnf-ir/src/registry/layout.rs` | rename file + types. |

---

## §2 — New-facility table (items not currently extant)

| # | New facility | Why it's missing | Located at | Implementation sketch | Locks / precepts honoured |
|---|---|---|---|---|---|
| 1 | `inverse-layout-audit` IR pass — every `->` annotation in every grammar reaches the tape emitter; build fails when a compound-typed rule has no resolvable `Layout` | RESTART-SKETCH §B.1 Layer 4 names this pass; it is the source-of-truth gate for `feedback_typed-materialization-invariant`. The audit at `crates/ir/src/passes/audit/payload_coverage.rs` is a coverage-report — not a build-failing pass | `bbnf-passes/src/audit/inverse_layout/` (sub-module of audit) | walk every rule's body; for every `IrNode::Map` whose `FnDescriptor` is typed-leaf-class, look up the enclosing rule's `Layout` from `LayoutRegistry`; if missing, emit a build error with rule-name + rule-source-span. Pass entry: `audit_inverse_layout(ir: &GrammarIR, registry: &LayoutRegistry) -> Result<(), Vec<InverseLayoutError>>`. The pass runs at the end of the codegen-time IR pipeline; failure aborts xtask-regen. | `feedback_typed-materialization-invariant`, Lock 2 (Layout canon), Lock § no-workarounds (no silent payload drop) |
| 2 | `bbnf-grammar` crate carrying the grammar-source-tree types + parser, separate from `bbnf-bootstrap` | today the BBNF grammar's `parse(source)` entry lives at `crates/core/src/grammar/mod.rs`, which is library-root + a hand-written entry that leaks input to satisfy a `'static` requirement. The grammar-types live at `crates/core/src/types.rs`. The conflation of "grammar-source types" with "library-root" is a Lock 13 mixed-concern. | `crates/bbnf-grammar/` (workspace member) | new crate carrying `AST<'a>`, `RuleEntry<'a>`, `ImportDirective<'a>`, plus the post-restart `parse_grammar(source: &str) -> ParsedGrammar<'_>` entry. The `bbnf-bootstrap` crate becomes a re-export shim or merges in. | Lock 13, Lock 11 (incubating boundary clarity) |
| 3 | Workspace metadata schema validator (xtask-side) | per Phase-3 surgery #16 (recogniser plugin schema fields), the workspace metadata for grammars + strategy + recognisers + shape templates needs schema validation. Today the validation is implicit (parse failure at `cargo build` time). The HARDENING-PLAN-SYNTHESIS punch list item #16 names this gap. | `xtask/src/validate/` | xtask command `cargo xtask validate-metadata` walks `[workspace.metadata.bbnf]`, `[workspace.metadata.bbnf-strategy]`, and (per replacement #2) `[workspace.metadata.bbnf.grammars.<g>.shape-templates]`; checks each row is well-formed; reports unknown fields, missing fields, type mismatches. CI gate: `cargo xtask validate-metadata --check`. | system-cohesion, no-workarounds (catch metadata drift early) |
| 4 | Unified `bbnf-error` crate carrying error types shared across parse / lower / codegen / runtime | today every crate carries its own `Error` type (`ImportError`, `PathError`, `ParseError`, ...); cross-crate composition forces conversions. A unified error crate would let consumers reason about a single error alphabet. | `crates/bbnf-error/` | trait `BbnfError` + per-crate impl; one canonical error wrapper that consumers down-stream can match on. The per-crate error types remain (per-domain ergonomics); the wrapper is the boundary type. | system-cohesion, KISS for consumers |
| 5 | Cohort-template generator (per Phase-3 gap D; HARDENING-PLAN-SYNTHESIS punch list #20) | the five trivial cohort grammars (BNF, CSV, EBNF, CSS Pretty, Math) have ~2 K LOC of byte-near-identical instantiation across `runtime/<g>/{arena,builder,document,kind,mod,value,view}.rs`. Generator-emitted instantiation eliminates the LOC. | `xtask/src/template/cohort/` (or proc-macro under `bbnf-codegen/`) | xtask consumes per-grammar metadata + the cohort-shape spec; emits 5 × 7 thin-instantiation files (~50 LOC each); existing hand-written ~1500 LOC of cohort runtime retires. | KISS, DRY, Lock 14 (template-emitted, not hand-written per-grammar) |
| 6 | Per-grammar declaration crate template | Lock 14 footnote names "optionally a per-grammar declaration crate (`crates/<grammar>/`) carrying host-fn implementations". Today no per-grammar declaration crate exists. The CSS L4 host fn at `css_types.rs` (replacement #1) is the trigger. | `crates/bbnf-grammar-<g>/` (per grammar that needs host fns) | thin crate template: `Cargo.toml` + `src/lib.rs` + `src/host.rs`; library re-exports per-grammar host fns. CSS L4 is the first; future grammars opt-in by adding the crate. | Lock 14 |
| 7 | Path executor surface for `crates/path/src/runtime/` | the executor today lives at `crates/core/src/path/executor.rs` (171 LOC); the post-Proposal-3 location is `path/src/runtime/executor.rs`. This is a *relocation*, not a new facility — but the API surface deserves explicit framing per Lock 7's footnote. | `path/src/runtime/` | re-locate cursor + executor + ascent + wildcard + variant_select. The runtime surface IS new in the sense that it's now at the canonical Lock 7 location. | Lock 7, KISS |
| 8 | `LayoutSink` trait | Lock 2 names "the trait that consumes [Layout] is `LayoutSink`". Today no `LayoutSink` trait exists — codegen consumes the layout via concrete type access. | `bbnf-ir/src/registry/sink.rs` | `pub trait LayoutSink { fn admit_layout(&mut self, layout: &Layout); fn admit_field(&mut self, ...); fn finish(self) -> Result<...>; }` — the trait every backend implements when it consumes the Layout. The Rust emitter, TS emitter, WASM emitter each impl `LayoutSink`. | Lock 2, Lock 5 |

---

## §3 — Sketches in greater detail

### §3.1 — Replacement #2 (BBNF shape miner generalisation) sketch

**Workspace metadata** (added to root Cargo.toml):

```toml
[workspace.metadata.bbnf.grammars.bbnf.shape-templates]
big_comment = { rule = "big_comment", body_pattern = "compound:Repeat", payload = "span" }
mapped_factor_empty = { rule = "mapped_factor", body_pattern = "compound:Optional@empty", payload = "none" }
```

**IR pass interface**:

```rust
// bbnf-passes/src/recognizers/structural_shape/mod.rs
pub fn mine_structural_shapes(
    ir: &GrammarIR,
    specs: &[ShapeTemplateSpec],
) -> Vec<DetectedShape>;

pub struct ShapeTemplateSpec {
    pub rule_pattern: String,    // glob or literal
    pub body_pattern: BodyPattern,
    pub payload_kind: PayloadKind,
}

pub enum BodyPattern {
    Compound(CompoundPattern),
    Repeat,
    Optional { empty: bool },
    Seq(Vec<BodyPattern>),
}

pub struct DetectedShape {
    pub rule_name: String,
    pub template_id: ShapeTemplateId,
    pub payload_layout: PayloadLayout,
}
```

**xtask-side wiring**:

```rust
// xtask/src/regen/structural_shapes.rs
let specs = parse_shape_templates(&workspace_metadata);
let detected = mine_structural_shapes(&ir, &specs);
// detected feeds into GrammarProfile.structural_shapes
```

The BBNF-specific knowledge (which rules, which body shape) is now data;
the pass is grammar-agnostic. Adding a new structural shape per grammar is
a metadata addition + zero source change.

### §3.2 — New facility #1 (inverse-layout-audit) sketch

**Pass entry**:

```rust
// bbnf-passes/src/audit/inverse_layout.rs
pub fn audit_inverse_layout(
    ir: &GrammarIR,
    registry: &LayoutRegistry,
) -> Result<(), Vec<InverseLayoutError>>;

pub struct InverseLayoutError {
    pub rule_id: RuleId,
    pub rule_name: String,
    pub source_span: GrammarSpan,
    pub issue: InverseLayoutIssue,
}

pub enum InverseLayoutIssue {
    NoLayoutForCompoundTypedRule { rule_name: String },
    LayoutDoesNotAdmitFnDescriptor { fn_kind: String, layout: String },
    MapTargetUnreachable { fn_kind: String, target_field: String },
}
```

**Walker shape**:

```rust
// for each rule with body { rule.body }:
//     for each IrNode::Map(_, fn_desc) in walk(rule.body):
//         if is_typed_arrow(fn_desc):
//             let layout = registry.lookup(rule.id).ok_or(NoLayoutForCompoundTypedRule)?;
//             if !layout.admits(fn_desc) {
//                 errors.push(LayoutDoesNotAdmitFnDescriptor);
//             }
```

**xtask-side gate**:

```bash
$ cargo xtask regen --check
# fails build if audit_inverse_layout returns errors
# exit 1 with structured stderr listing each rule + span + issue
```

This pass IS the "every `->` reaches the tape emitter" gate — the
substantive realisation of `feedback_typed-materialization-invariant`. It
is named in RESTART-SKETCH §B.1 Layer 4 and HARDENING-PLAN-SYNTHESIS
punch list item #17.

### §3.3 — New facility #5 (cohort-template generator) sketch

**Spec file** (per `xtask/src/template/cohort/spec.toml` or workspace-
metadata):

```toml
[cohort-grammars]
members = ["bnf", "csv", "ebnf", "css_pretty", "math"]
template = "simple"

[cohort-template.simple]
arena = "templates/arena.rs.tt"
builder = "templates/builder.rs.tt"
document = "templates/document.rs.tt"
kind = "templates/kind.rs.tt"
mod = "templates/mod.rs.tt"
value = "templates/value.rs.tt"
view = "templates/view.rs.tt"
```

**Generation entry**:

```rust
// xtask/src/template/cohort/mod.rs
pub fn regen_cohort(spec: &CohortSpec, ir_set: &[GrammarIR]) -> Result<(), Error> {
    for grammar in &spec.members {
        let ir = ir_set.iter().find(|i| i.name == *grammar).expect("cohort member missing IR");
        for (file, template_path) in &spec.template_files {
            let rendered = render(template_path, &TemplateCtx::from_ir(ir));
            let dest = format!("crates/<runtime>/src/{}/{}.rs", grammar, file);
            write_atomically(dest, rendered)?;
        }
    }
}
```

**LOC budget**:
- pre: 5 grammars × 7 files × ~50-200 LOC = ~1500 LOC hand-written
- post: 5 grammars × 7 files × ~30 LOC each (thin instantiation) +
  ~200 LOC of templates = ~1250 LOC total
- net: ~250 LOC saved + the structural assurance that adding a 6th cohort
  member is a metadata change + a template re-render

### §3.4 — New facility #4 (`bbnf-error` crate) sketch

**Crate manifest**:

```toml
# crates/bbnf-error/Cargo.toml
[package]
name = "bbnf-error"
version = "0.1.0"
```

**Trait + canonical wrapper**:

```rust
// crates/bbnf-error/src/lib.rs
pub trait BbnfError: std::error::Error + Send + Sync + 'static {
    fn span(&self) -> Option<GrammarSpan>;
    fn category(&self) -> ErrorCategory;
}

pub enum ErrorCategory {
    Import,         // ImportError
    Lower,          // LowerError
    Layout,         // LayoutError (post Lock 2)
    Codegen,        // CodegenError
    Runtime,        // RuntimeError
    Path,           // PathError
}

pub struct BbnfErrorWrapper(pub Box<dyn BbnfError>);
```

Per-crate error types remain (e.g. `ImportError`); they `impl BbnfError`.
Cross-crate composition matches on `ErrorCategory`. Consumers reach a
unified surface; per-domain ergonomics survive.

---

## §4 — Items intentionally NOT replaced

The lane intentionally does NOT design replacements for:

- The `Box::leak` in `crates/core/src/grammar/mod.rs:57` — Pass A flags
  it; the resolution requires a public-API call (force callers to provide
  `&'static`-able input vs introduce `parse_in(input, &bump)`). Synthesizer
  adjudicates.
- The TS/WASM backend slots in `EmitStrategy::StructDirect.{ts, wasm}` —
  these are reserved for BD+ landing per Lock 5 and are out of Pass A
  scope.
- The dev binaries `dump_ir.rs`, `cost_grid_sweep.rs`, `debug_parse.rs` —
  these survive in their current form whether `crates/bootstrap/` retires
  or not (Proposal 6 territory).

---

## §5 — Cross-reference to Pass B / Pass C

Pass A surfaces these residues to flag for synthesizer:

- **Replacement #5 (cohort-template generator)** spans Pass A (the
  metadata schema + xtask command) and Pass B (the per-grammar runtime
  emit). Pass B owns the runtime side.
- **Replacement #7 (per-grammar emitted REGISTRY)** spans Pass A (the
  consumer side) and Pass B (the emit side). Pass B owns the emit.
- **Replacement #11 (BBNF aggregator deletion)** spans Pass A (the
  generated/mod.rs entry) and Pass B (the codegen output discipline). Pass
  B's codegen ensures every grammar's namespacing is uniform.
- **New facility #4 (`bbnf-error` crate)** spans all three passes — the
  trait + wrapper define a cross-cutting surface. Synthesizer ratifies.
- **Per-grammar declaration crate template (new facility #6)** is the
  Lock 14 declaration-crate substrate; its existence is a Pass A finding,
  but instantiation across grammars is per-grammar-driven.

---

## §6 — Aggregate counts

| Surface | Count |
|---|---:|
| Replacements (ABROGATE-REPLACE) | 18 |
| New facilities | 8 |
| Items deferred to synthesizer adjudication | 3 |
| Pass A residues for synthesizer | 5 |
