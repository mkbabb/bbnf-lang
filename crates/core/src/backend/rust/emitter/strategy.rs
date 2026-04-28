//! AZ-I.W2.RA — `EmitStrategy` — per-grammar codegen-time substrate
//! selector for the Rust backend's `parse()` body.
//!
//! The Rust emitter writes one of two disjoint parse-fn bodies per
//! grammar:
//!
//! - [`EmitStrategy::TapeDirect`] — the legacy fused `Tape<()>` path.
//!   The dispatcher writes structural columns + paired value frames
//!   into a single `Tape<R>` substrate, finalises with `Tape::finish`,
//!   and returns `Parsed<Self>`.
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
//! the fully-qualified `builder_path` / `document_path` strings as
//! `&'static str` data; future grammars (Sheets in W2.B, CSS L4 in
//! W3) extend the resolver match by adding new arms — they do not
//! modify existing call sites.
//!
//! # Wire contract
//!
//! `for_grammar(grammar_ident, &registry)` is invoked by:
//!
//! - `pipeline::compile::finalize_compile` — to record the resolved
//!   strategy alongside the `PreparedGrammar` (so consumers that
//!   coexist with multiple grammars in one binary can disambiguate).
//! - `backend::rust::emitter::shapes::emit_shapes_for_grammar` — to
//!   thread `&EmitStrategy` to per-shape emitter call sites (B/C/D/E
//!   pull it through the per-shape body in stage 2).
//! - `backend::rust::emitter::grammar::emit_grammar_impl` — to choose
//!   the matching `parse_body` arm.
//!
//! The resolver is the single decision surface; per-shape emitters
//! consume the result, they do not re-derive it.

use bbnf_ir::registry::StructRegistry;

/// Per-grammar codegen-time substrate selector.
///
/// Variants are data, not behaviour: the `&'static str` payloads
/// carry fully-qualified type paths the emitter splices into the
/// generated `parse()` body. Adding a new struct-builder grammar
/// (Sheets, CSS L4) extends the [`EmitStrategy::for_grammar`]
/// resolver with a new arm — no existing arm changes.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum EmitStrategy {
    /// Generated `parse()` builds the grammar's typed document via a
    /// concrete `StructBuilder` impl.
    ///
    /// `builder_path` — fully-qualified type path the emitter
    /// instantiates with `<builder_path>::new()` (e.g.
    /// `"::bbnf::runtime::json::JsonStructBuilder"`).
    ///
    /// `document_path` — fully-qualified type path the emitter
    /// returns from `parse()` (e.g.
    /// `"::bbnf::runtime::json::JsonDocument"`). Same lifetime
    /// signature as the builder; per-grammar code threads `'p` for
    /// arena-borrowed slices.
    StructDirect {
        builder_path: &'static str,
        document_path: &'static str,
    },
    /// Generated `parse()` writes through the `Tape<()>` substrate
    /// and returns `Parsed<Self>`. The pre-AZ-I.W2 default for every
    /// grammar; remains the default for BBNF, BNF, EBNF, CSV, math
    /// throughout AZ-I.
    TapeDirect,
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
    /// requires a populated registry — an empty registry on a
    /// nominally struct-direct grammar resolves to [`Self::TapeDirect`]
    /// rather than emitting a structurally-broken parse body.
    ///
    /// # AZ-I.W2 admission rules
    ///
    /// - JSON (parser ident `"JsonParser"`, plus the historical alias
    ///   `"JsonGrammar"` per the W2-EMITTER-REWIRE plan §1) → struct-
    ///   direct via `JsonStructBuilder` / `JsonDocument`.
    /// - Every other grammar → tape-direct.
    ///
    /// # AZ-I.W2.B / AZ-I.W3 extensions
    ///
    /// Sheets (W2.B) and CSS L4 (W3) extend this resolver with their
    /// own struct-builder arms. The match is exhaustive in spirit —
    /// every newly-admitted grammar carries a positive arm here,
    /// the negative default falls through to tape.
    pub fn for_grammar(grammar_ident: &str, registry: &StructRegistry) -> Self {
        // The struct-direct path requires the registry to carry at
        // least one layout — projection ran and produced something.
        // An empty registry signals project_types saw no Named rules
        // worth recording; downgrade to tape rather than emit a
        // body that calls `begin_compound` against a missing layout.
        let registry_populated = !registry.is_empty();

        match (grammar_ident, registry_populated) {
            // JSON — primary AZ-I.W2 activation target. Both the
            // generated parser ident (`JsonParser`) and the plan's
            // nominal name (`JsonGrammar`) admit; the latter exists
            // for forward-compat with hand-written test fixtures.
            ("JsonParser" | "JsonGrammar", true) => EmitStrategy::StructDirect {
                builder_path: "::bbnf::runtime::JsonStructBuilder",
                document_path: "::bbnf::runtime::JsonDocument",
            },
            // Everything else — tape-direct. Includes BBNF / BNF /
            // EBNF / CSV / math / CSS / Sheets pre-W2.B / W3.
            _ => EmitStrategy::TapeDirect,
        }
    }

    /// Returns `true` when the strategy emits the struct-direct
    /// parse-body path. Used by the dispatcher and `parse_body`
    /// arms to pick the matching emission template.
    #[inline]
    pub fn is_struct_direct(&self) -> bool {
        matches!(self, EmitStrategy::StructDirect { .. })
    }

    /// Returns `true` when the strategy emits the legacy tape-direct
    /// parse-body path.
    #[inline]
    pub fn is_tape_direct(&self) -> bool {
        matches!(self, EmitStrategy::TapeDirect)
    }
}
