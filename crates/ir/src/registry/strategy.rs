//! AZ-I.W2-act.A — `EmitStrategy` — IR-level codegen-time substrate
//! selector.
//!
//! Per `audit/AUDIT-6-ARCHITECTURE.md` §4 + §8.1, the substrate-
//! selection decision is **backend-shared, not Rust-specific**. The
//! pre-W2-act home at `crates/core/src/backend/rust/emitter/strategy.rs`
//! coupled the selector to the Rust backend and would have forced
//! per-backend duplicates when TS / WASM host bindings activate the
//! same struct-direct grammar at BA. Hoisting the enum + resolver to
//! `bbnf_ir::registry::strategy` makes the decision native at the IR
//! layer; each backend reads the resolved [`EmitStrategy`] off the
//! prepared grammar and consults its own [`SubstrateBinding`] field
//! ([`SubstrateBinding::rust`] today; `ts` / `wasm` populated at
//! BA-host-bindings time).
//!
//! The Rust emitter writes the struct-builder parse-fn body for every
//! production grammar:
//!
//! - [`EmitStrategy::StructDirect`] — the AZ-I.W2 struct-builder path.
//!   The dispatcher writes typed compound / leaf records into a
//!   grammar-specific concrete `StructBuilder` (e.g. `JsonStructBuilder`)
//!   and returns the matching grammar-specific document type
//!   (e.g. `JsonDocument`).
//!
//! Selection happens at codegen time, not at runtime —
//! `feedback_no-orthogonal-codepaths` is in force: ONE codegen path
//! per grammar; the dispatch is data, not branches threaded through
//! every emitted shape body.
//!
//! # Pluggability
//!
//! Per `feedback_pluggable-components`, the resolver
//! [`EmitStrategy::for_grammar`] is data-driven: the variant carries
//! per-backend [`SubstrateBinding`] records as `&'static str` data;
//! future grammars extend the resolver match by adding new arms —
//! they do not modify existing call sites.
//!
//! # Wire contract
//!
//! `for_grammar(grammar_ident, &registry)` is invoked by:
//!
//! - `pipeline::compile::resolve_emit_strategy` — the pipeline-level
//!   adapter so test harnesses can drive the resolver without reaching
//!   into the backend module path directly.
//! - `backend::rust::emitter::shapes::emit_shapes_for_grammar` — to
//!   thread `&EmitStrategy` to per-shape emitter call sites.
//! - `backend::rust::emitter::grammar::emit_grammar_impl` — to choose
//!   the matching `parse_body` arm.
//!
//! The resolver is the single decision surface; per-shape emitters
//! consume the result, they do not re-derive it.

use crate::registry::StructRegistry;

/// Per-backend binding describing where the codegen-time substrate
/// resolves on a given backend.
///
/// Carries fully-qualified type-path strings the emitter splices into
/// the generated `parse()` body. The Rust backend consumes
/// [`SubstrateBinding::builder_path`] / [`SubstrateBinding::document_path`]
/// directly; the TS / WASM hosts (BA wave) read the same field
/// shapes, mapped per-binding to that backend's native module path.
///
/// # Field semantics
///
/// - `builder_path` — Rust path the emitter instantiates with
///   `<builder_path>::new()` (e.g.
///   `"crate::runtime::json::JsonStructBuilder"`). Generated code
///   lives inside the `bbnf` crate, so paths are rooted at `crate::`
///   (B5 retired the `extern crate self as bbnf` self-alias; an
///   absolute `::bbnf::` path does not resolve from inside the crate).
/// - `document_path` — Rust path the emitter returns from `parse()`
///   (e.g. `"crate::runtime::json::JsonDocument"`). Same lifetime
///   signature as the builder; per-grammar code threads `'p` for
///   arena-borrowed slices.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct SubstrateBinding {
    /// Rust path of the grammar's `StructBuilder` implementor — e.g.
    /// `"crate::runtime::json::JsonStructBuilder"`.
    pub builder_path: &'static str,
    /// Rust path of the grammar's `Document` return type — e.g.
    /// `"crate::runtime::json::JsonDocument"`.
    pub document_path: &'static str,
}

/// Per-grammar codegen-time substrate selector.
///
/// Variants are data, not behaviour: the [`SubstrateBinding`] payloads
/// carry fully-qualified type paths the emitter splices into the
/// generated `parse()` body. Adding a new struct-builder grammar
/// (Sheets, CSS L4) extends the [`EmitStrategy::for_grammar`]
/// resolver with a new arm — no existing arm changes.
///
/// # Per-backend bindings
///
/// The `rust` field is populated for every active struct-direct
/// grammar; `ts` / `wasm` are reserved for BA host-bindings when
/// the per-backend native runtime types land. Today's resolver
/// returns `None` for `ts` / `wasm` on every arm — the BA wave
/// extends the resolver with backend-specific bindings; backends
/// failing to find their slot fail codegen loudly; there is no
/// production fallback substrate after AZ-II.cutover.O4.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum EmitStrategy {
    /// Generated `parse()` builds the grammar's typed document via a
    /// concrete `StructBuilder` impl.
    StructDirect {
        /// Rust-backend binding — consumed by
        /// `crates/core/src/backend/rust/emitter/`.
        rust: SubstrateBinding,
        /// TS-backend binding — reserved for BA host bindings; the
        /// per-grammar resolver arm fills this when the TS runtime
        /// types land.
        ts: Option<SubstrateBinding>,
        /// WASM-backend binding — reserved for BA host bindings;
        /// same dynamic as `ts`.
        wasm: Option<SubstrateBinding>,
    },
}

/// AZ-IV.W1.8 — canonical manifest-driven strategy table.
///
/// Per Fermat hardening pass §F5 + AZ-IV §Hard Gates 18 + 22, the
/// per-grammar strategy binding is **data**, not a Rust source-side
/// arm-list. Every production grammar that activates StructDirect
/// contributes one row here; adding a new grammar adds a row, not a
/// match arm in `for_grammar`. The table mirrors
/// `[workspace.metadata.bbnf-strategy]` in the workspace `Cargo.toml`
/// (the `regen --check` gate wires the two sources together).
///
/// Each row's `idents` slice carries every parser-struct alias the
/// grammar advertises (so `"JsonParser"` and `"JsonGrammar"`
/// continue to resolve to the same binding without a literal arm).
pub const PRODUCTION_MANIFEST_TABLE: &[ManifestStrategyEntry] = &[
    // AZ-I.W2-act.B1 — JSON.
    ManifestStrategyEntry {
        idents: &["JsonParser", "JsonGrammar"],
        rust_builder_path: "crate::runtime::json::JsonStructBuilder",
        rust_document_path: "crate::runtime::json::JsonDocument",
    },
    // AZ-I.W2-act.B2 — Google Sheets.
    ManifestStrategyEntry {
        idents: &["GoogleSheetsParser", "GoogleSheetsGrammar"],
        rust_builder_path: "crate::runtime::google_sheets::SheetsStructBuilder",
        rust_document_path: "crate::runtime::google_sheets::SheetsDocument",
    },
    // AZ-I.W2-act.B3 — CSS L4.
    ManifestStrategyEntry {
        idents: &["CssL4Parser"],
        rust_builder_path: "crate::runtime::css_l4::CssStructBuilder",
        rust_document_path: "crate::runtime::css_l4::CssDocument",
    },
    // AZ-II.cutover.H — BBNF self-host.
    ManifestStrategyEntry {
        idents: &["BbnfBootstrap", "BbnfParser"],
        rust_builder_path: "crate::runtime::bbnf::BbnfStructBuilder",
        rust_document_path: "crate::runtime::bbnf::BbnfDocument",
    },
    // AZ-II.cutover.L Phase 3c — CSV.
    ManifestStrategyEntry {
        idents: &["CsvParser", "CsvGrammar"],
        rust_builder_path: "crate::runtime::csv::CsvStructBuilder",
        rust_document_path: "crate::runtime::csv::CsvDocument",
    },
    // AZ-II.cutover.L Phase 3c — Math.
    ManifestStrategyEntry {
        idents: &["MathParser", "MathGrammar"],
        rust_builder_path: "crate::runtime::math::MathStructBuilder",
        rust_document_path: "crate::runtime::math::MathDocument",
    },
    // AZ-II.cutover.L Phase 3c — BNF.
    ManifestStrategyEntry {
        idents: &["BnfParser", "BnfGrammar"],
        rust_builder_path: "crate::runtime::bnf::BnfStructBuilder",
        rust_document_path: "crate::runtime::bnf::BnfDocument",
    },
    // AZ-II.cutover.O.2 — EBNF.
    ManifestStrategyEntry {
        idents: &["EbnfParser", "EbnfGrammar"],
        rust_builder_path: "crate::runtime::ebnf::EbnfStructBuilder",
        rust_document_path: "crate::runtime::ebnf::EbnfDocument",
    },
    // AZ-II.cutover.L Phase 3c — CSS Pretty.
    ManifestStrategyEntry {
        idents: &["CssPrettyParser", "CssPrettyGrammar"],
        rust_builder_path: "crate::runtime::css_pretty::CssPrettyStructBuilder",
        rust_document_path: "crate::runtime::css_pretty::CssPrettyDocument",
    },
];

impl EmitStrategy {
    /// Resolve the codegen substrate for `grammar_ident` against the
    /// canonical [`PRODUCTION_MANIFEST_TABLE`].
    ///
    /// `grammar_ident` is the parser-struct identifier the emitter
    /// passes to `emit_shapes_for_grammar` — the literal Rust ident
    /// that names the generated parser (e.g. `"JsonParser"`,
    /// `"BbnfBootstrap"`, `"CssL4Parser"`).
    ///
    /// `registry` is the rule-id → layout map populated by
    /// `bbnf_ir::passes::project_types`. The struct-direct path
    /// requires a populated registry; an empty registry is a hard
    /// generation error rather than a tape fallback.
    ///
    /// # AZ-IV.W1.8 — manifest-driven binding
    ///
    /// Pre-W1.8 this method shipped a 9-arm match expression listing
    /// every production grammar by literal parser-name. Per Fermat
    /// §F5 + AZ-IV §Hard Gates 18 the arm-list violated grammar
    /// generality (a synthetic grammar rename test fails if a new
    /// arm is required). The arm-list is replaced with a single
    /// lookup against [`PRODUCTION_MANIFEST_TABLE`] — the data is
    /// the discriminator, not behaviour. Adding a grammar adds one
    /// row; no source-side dispatch arm changes.
    pub fn for_grammar(grammar_ident: &str, registry: &StructRegistry) -> Self {
        Self::for_grammar_with_manifest(grammar_ident, registry, PRODUCTION_MANIFEST_TABLE)
    }

    /// Returns `true` when the strategy emits the struct-direct
    /// parse-body path. Used by the dispatcher and `parse_body`
    /// arms to pick the matching emission template.
    #[inline]
    pub fn is_struct_direct(&self) -> bool {
        matches!(self, EmitStrategy::StructDirect { .. })
    }

    /// AZ-IV.W0.3 / W1.8 — manifest-driven strategy resolver.
    ///
    /// Looks up `grammar_ident` in `manifest_table` (per-row entries
    /// constructed from `[workspace.metadata.bbnf-strategy]` or the
    /// canonical [`PRODUCTION_MANIFEST_TABLE`]). When a row matches,
    /// the strategy is constructed directly from the row's Rust
    /// binding paths — no literal arm in this source file is
    /// consulted. When no row matches, the call panics with a
    /// structured error naming the offending grammar ident.
    ///
    /// `registry` is the rule-id → layout map populated by
    /// `bbnf_ir::passes::project_types`. The struct-direct path
    /// requires a populated registry; an empty registry is a hard
    /// generation error rather than a tape fallback.
    ///
    /// W1.8 closes the W0.3 scaffold by routing
    /// [`EmitStrategy::for_grammar`] through this resolver against
    /// [`PRODUCTION_MANIFEST_TABLE`]. The synthetic-grammar test
    /// (`crates/core/tests/synthetic_grammar_strategy.rs`) registers
    /// a row and resolves it without needing a new literal arm to
    /// land — proving manifest-only round-trip per AZ-IV §Hard
    /// Gates 22.
    pub fn for_grammar_with_manifest(
        grammar_ident: &str,
        registry: &StructRegistry,
        manifest_table: &[ManifestStrategyEntry],
    ) -> Self {
        if registry.is_empty() {
            panic!(
                "EmitStrategy::for_grammar_with_manifest: `{grammar_ident}` has an empty StructRegistry; \
                 StructDirect generation requires project_types registry closure"
            );
        }

        if let Some(row) = manifest_table
            .iter()
            .find(|entry| entry.matches_ident(grammar_ident))
        {
            return EmitStrategy::StructDirect {
                rust: SubstrateBinding {
                    builder_path: row.rust_builder_path,
                    document_path: row.rust_document_path,
                },
                ts: None,
                wasm: None,
            };
        }

        panic!(
            "EmitStrategy::for_grammar_with_manifest: unknown production grammar `{grammar_ident}`; \
             add a row to PRODUCTION_MANIFEST_TABLE in crates/ir/src/registry/strategy.rs \
             (or thread a fixture manifest table for synthetic-grammar tests). \
             Per AZ-IV §Hard Gates 18, parser strategy binding is manifest/registry driven; \
             a missing entry is a manifest defect, not a runtime fallback."
        );
    }
}

/// AZ-IV.W0.3 — one row of the manifest-driven strategy table.
///
/// Per Fermat hardening pass (`docs/tranches/AZ-IV/audit/HARDENING-2026-05-01-fermat.md`
/// §F5), the literal parser-name arm-list at
/// [`EmitStrategy::for_grammar`] must be replaced with manifest-driven
/// data. This row carries the same payload an arm carries today,
/// keyed by every parser-ident alias the grammar advertises (so
/// `"JsonParser"` and `"JsonGrammar"` continue to resolve to the same
/// binding without a literal arm in source).
///
/// Rows are constructed by the manifest reader (consumer side, not
/// this crate). The reader normalises rows from
/// `[workspace.metadata.bbnf-strategy]` or an in-memory test fixture;
/// either source produces the same row shape, so the resolver is
/// identical regardless of provenance.
///
/// # Field semantics
///
/// - `idents` — every parser-struct ident the grammar registers. The
///   resolver matches if `grammar_ident` is in this slice; the slice
///   is `&'static [&'static str]` so rows live in the consumer's
///   static memory without per-call allocation.
/// - `rust_builder_path` / `rust_document_path` — same shape as
///   [`SubstrateBinding`]'s fields; spliced verbatim into the
///   generated `parse()` body when this row matches.
///
/// W1.5/W1.8 extends the row with `ts` / `wasm` substrate bindings
/// once the host-binding template lands.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ManifestStrategyEntry {
    /// Parser-struct idents the grammar registers (e.g. `&["JsonParser",
    /// "JsonGrammar"]`). The resolver matches when `grammar_ident` is
    /// in the slice.
    pub idents: &'static [&'static str],
    /// Rust-backend `StructBuilder` path — same shape as
    /// [`SubstrateBinding::builder_path`].
    pub rust_builder_path: &'static str,
    /// Rust-backend `Document` path — same shape as
    /// [`SubstrateBinding::document_path`].
    pub rust_document_path: &'static str,
}

impl ManifestStrategyEntry {
    /// True when `ident` is one of the parser-struct names this row
    /// covers.
    #[inline]
    pub fn matches_ident(&self, ident: &str) -> bool {
        self.idents.iter().any(|alias| *alias == ident)
    }
}
