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

impl EmitStrategy {
    /// Resolve the codegen substrate for `grammar_ident`.
    ///
    /// `grammar_ident` is the parser-struct identifier the emitter
    /// passes to `emit_shapes_for_grammar` — the literal Rust ident
    /// that names the generated parser (e.g. `"JsonParser"`,
    /// `"BbnfBootstrap"`, `"CssL4Parser"`). The resolver matches on
    /// the canonical idents the bootstrap regen produces (see
    /// `crates/core/src/grammar/generated/`).
    ///
    /// `registry` is the rule-id → layout map populated by
    /// `bbnf_ir::passes::project_types`. The struct-direct path
    /// requires a populated registry; an empty registry is a hard
    /// generation error rather than a tape fallback.
    ///
    /// # AZ-I.W2-act admission rules
    ///
    /// - AZ-I.W2-act introduced per-grammar positive arms.
    /// - AZ-II.cutover.A through O2 activated every production
    ///   grammar onto StructDirect.
    /// - AZ-II.cutover.O4 removes the catch-all tape fallback; unknown
    ///   grammars and empty registries fail loudly.
    pub fn for_grammar(grammar_ident: &str, registry: &StructRegistry) -> Self {
        if registry.is_empty() {
            panic!(
                "EmitStrategy::for_grammar: `{grammar_ident}` has an empty StructRegistry; \
                 StructDirect generation requires project_types registry closure"
            );
        }

        match grammar_ident {
            // AZ-I.W2-act.B1: JSON activates onto the struct-direct
            // path. The grammar-emitted `JsonParser::parse` returns
            // `Result<JsonDocument<'_>, ParseErr>` after the
            // orchestrator's post-flip regen consumes this strategy.
            "JsonParser" | "JsonGrammar" => EmitStrategy::StructDirect {
                rust: SubstrateBinding {
                    builder_path: "crate::runtime::json::JsonStructBuilder",
                    document_path: "crate::runtime::json::JsonDocument",
                },
                ts: None,
                wasm: None,
            },
            // AZ-I.W2-act.B2: Google Sheets struct-direct activation.
            "GoogleSheetsParser" | "GoogleSheetsGrammar" => EmitStrategy::StructDirect {
                rust: SubstrateBinding {
                    builder_path: "crate::runtime::google_sheets::SheetsStructBuilder",
                    document_path: "crate::runtime::google_sheets::SheetsDocument",
                },
                ts: None,
                wasm: None,
            },
            // AZ-I.W2-act.B3: CSS L4 struct-direct activation. The CSS
            // L4 grammar projects through the `bbnf::runtime::css_l4`
            // typed-value enum family + `CssStructBuilder` /
            // `CssDocument` substrate authored at W2-act.B3.
            "CssL4Parser" => EmitStrategy::StructDirect {
                rust: SubstrateBinding {
                    builder_path: "crate::runtime::css_l4::CssStructBuilder",
                    document_path: "crate::runtime::css_l4::CssDocument",
                },
                ts: None,
                wasm: None,
            },
            // AZ-II.cutover.H — BBNF struct-direct activation. cutover.F
            // landed the emitter-side fixes (Array Shape-2 dispatch +
            // Flat Alt/Repeat/Regex/Negate/Minus inline emission);
            // cutover.G landed the bootstrap-parser break-and-regen
            // substrate; cutover.H Phase 0 landed the validator
            // value-expr-subtree skip in `graph::deps`; cutover.H
            // Phase 1 lands the transparent-rule emitter fix at
            // `shapes/mod.rs:202` together with this resolver-arm
            // re-flip. The regen pipeline is now self-hosting via the
            // generated parser.
            "BbnfBootstrap" | "BbnfParser" => EmitStrategy::StructDirect {
                rust: SubstrateBinding {
                    builder_path: "crate::runtime::bbnf::BbnfStructBuilder",
                    document_path: "crate::runtime::bbnf::BbnfDocument",
                },
                ts: None,
                wasm: None,
            },
            // AZ-II.cutover.L Phase 3c — non-BBNF struct-direct
            // activation. cutover.E deferred these arms until the BBNF
            // struct-direct emitter regression was repaired; cutover.K
            // Phases 0-2 + cutover.L Phase 3a closed the chicken-and-egg
            // (BbnfBootstrap::parse self-hosts; pseudoClass /
            // pseudoElement Alt-of-Ref dispatch lands). The remaining
            // five arms now activate together — every grammar's
            // substrate types (`<Grammar>StructBuilder` / `<Grammar>Document`)
            // already exist under `crates/core/src/runtime/<grammar>/`.
            "CsvParser" | "CsvGrammar" => EmitStrategy::StructDirect {
                rust: SubstrateBinding {
                    builder_path: "crate::runtime::csv::CsvStructBuilder",
                    document_path: "crate::runtime::csv::CsvDocument",
                },
                ts: None,
                wasm: None,
            },
            "MathParser" | "MathGrammar" => EmitStrategy::StructDirect {
                rust: SubstrateBinding {
                    builder_path: "crate::runtime::math::MathStructBuilder",
                    document_path: "crate::runtime::math::MathDocument",
                },
                ts: None,
                wasm: None,
            },
            "BnfParser" | "BnfGrammar" => EmitStrategy::StructDirect {
                rust: SubstrateBinding {
                    builder_path: "crate::runtime::bnf::BnfStructBuilder",
                    document_path: "crate::runtime::bnf::BnfDocument",
                },
                ts: None,
                wasm: None,
            },
            // AZ-II.cutover.O.2 — EBNF struct-direct activation. O1
            // landed transactional StructDirect builders, so the
            // high-branch `letter` / `digit` / `symbol` alternate
            // attempts can now speculatively mutate builder state
            // without leaking failed branches into parent layouts.
            "EbnfParser" | "EbnfGrammar" => EmitStrategy::StructDirect {
                rust: SubstrateBinding {
                    builder_path: "crate::runtime::ebnf::EbnfStructBuilder",
                    document_path: "crate::runtime::ebnf::EbnfDocument",
                },
                ts: None,
                wasm: None,
            },
            "CssPrettyParser" | "CssPrettyGrammar" => EmitStrategy::StructDirect {
                rust: SubstrateBinding {
                    builder_path: "crate::runtime::css_pretty::CssPrettyStructBuilder",
                    document_path: "crate::runtime::css_pretty::CssPrettyDocument",
                },
                ts: None,
                wasm: None,
            },
            _ => panic!(
                "EmitStrategy::for_grammar: unknown production grammar `{grammar_ident}`; \
                 add an explicit StructDirect substrate binding"
            ),
        }
    }

    /// Returns `true` when the strategy emits the struct-direct
    /// parse-body path. Used by the dispatcher and `parse_body`
    /// arms to pick the matching emission template.
    #[inline]
    pub fn is_struct_direct(&self) -> bool {
        matches!(self, EmitStrategy::StructDirect { .. })
    }
}
