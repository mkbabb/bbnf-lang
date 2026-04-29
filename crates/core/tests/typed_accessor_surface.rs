//! AX.W1r.6 — Typed-accessor surface audit over 6 grammars.
//!
//! Formalizes the W0a.2.p/q cascade's typed accessor emission as a
//! regression gate. For every non-transparent rule in each of the six
//! gorgeous-backed grammars (JSON, CSS L4, Sheets, BBNF, EBNF, BNF),
//! this harness walks `ir.rules` + `ir.types` + `ir.payload_layouts`
//! and classifies the rule into the accessor class the view emitter
//! SHOULD produce. The classification mirrors
//! `crates/core/src/backend/rust/view/mod.rs::emit_typed_accessors`
//! (the dispatch function itself) — every branch in that dispatcher
//! lights up at least one rule in at least one grammar under this
//! audit, so future codegen regressions that silently drop an
//! accessor class surface here as a coverage-zero delta.
//!
//! # Layered assertions
//!
//! Layer 1 — **compile-time smoke** via the generated parser entry
//! points and document-owned runtime surfaces. StructDirect grammars
//! must compile without generated `NodeView`, `Root::View`, or
//! `ValueRoot` surfaces; each grammar test calls representative
//! document/view accessors (`view()`, `kind()`, `root()`, `arena()`,
//! `children()`, `to_value()`) on the runtime document returned by
//! `parse`.
//!
//! Layer 2 — **runtime IR audit**: for each grammar, we classify every
//! non-transparent rule by the accessor class the emitter's dispatcher
//! would route it into. The audit reports per-grammar coverage
//! numbers; the aggregate assertion is that every accessor class has
//! non-zero rule coverage across the six-grammar corpus (i.e. every
//! emitter branch is reached).
//!
//! # Qualifying conditions (from the dispatcher)
//!
//! 1. **Aggregate / KV-pair** (`ir.payload_layouts` hit):
//!    - KV-pair shape `(Span, scalar)` → `.key()` + `.value()`
//!    - Other aggregate → `.value() -> tuple` (+ optional
//!      `.as_color()` / `.try_as_color()` on Color / ColorMix rules)
//! 2. **Leaf scalar** (`TypeDesc::is_scalar_payload`):
//!    - `.text()` always, `.value()` + `.as_<T>()` when scalar.
//! 3. **Seq** (body is Seq):
//!    - `.child_N()` per effective child position + named refs.
//! 4. **Alt** (body is Alt):
//!    - `.as_<variant>()` / `.is_<variant>()` per branch + `.chosen()`.
//!    - `.value() -> <RuleName>Value` enum when every branch is
//!      payload-eligible (AQ.6.C typed-enum path).
//! 5. **Repeat** (body is Repeat):
//!    - `.iter()` / `.len()` / `.is_empty()` / `.get(i)`.
//!
//! Transparent rules (`rule.meta.is_transparent`) emit no view type
//! and are skipped from the audit.

use bbnf::pipeline::{
    compile_paths_request, CompileOutput, CompileRequest, CompileTarget, PipelineOptions,
};
use bbnf_ir::passes::is_kv_pair_shape;
use bbnf_ir::{GrammarIR, IrNode, IrRule, TypeDesc};
use std::path::PathBuf;

// ───────────────────────────────────────────────────────────────────
// Host shims required by the CSS L4 grammar's `-> parse_hex_color(...)`.
// Duplicated verbatim from `css_l4_color_view.rs` so this test compiles
// hermetically without cross-file coupling.
// ───────────────────────────────────────────────────────────────────

#[allow(dead_code)]
mod css_types {
    pub fn parse_hex_color(s: &str) -> u32 {
        let hex = s.as_bytes();
        match hex.len() {
            3 => {
                let r = hex_digit(hex[0]);
                let g = hex_digit(hex[1]);
                let b = hex_digit(hex[2]);
                ((r << 4 | r) << 24) | ((g << 4 | g) << 16) | ((b << 4 | b) << 8) | 0xFF
            }
            4 => {
                let r = hex_digit(hex[0]);
                let g = hex_digit(hex[1]);
                let b = hex_digit(hex[2]);
                let a = hex_digit(hex[3]);
                ((r << 4 | r) << 24) | ((g << 4 | g) << 16) | ((b << 4 | b) << 8) | (a << 4 | a)
            }
            6 => {
                let r = hex_byte(hex[0], hex[1]);
                let g = hex_byte(hex[2], hex[3]);
                let b = hex_byte(hex[4], hex[5]);
                (r << 24) | (g << 16) | (b << 8) | 0xFF
            }
            8 => {
                let r = hex_byte(hex[0], hex[1]);
                let g = hex_byte(hex[2], hex[3]);
                let b = hex_byte(hex[4], hex[5]);
                let a = hex_byte(hex[6], hex[7]);
                (r << 24) | (g << 16) | (b << 8) | a
            }
            _ => 0,
        }
    }

    #[inline(always)]
    fn hex_digit(b: u8) -> u32 {
        match b {
            b'0'..=b'9' => (b - b'0') as u32,
            b'a'..=b'f' => (b - b'a' + 10) as u32,
            b'A'..=b'F' => (b - b'A' + 10) as u32,
            _ => 0,
        }
    }

    #[inline(always)]
    fn hex_byte(hi: u8, lo: u8) -> u32 {
        (hex_digit(hi) << 4) | hex_digit(lo)
    }
}

// ───────────────────────────────────────────────────────────────────
// Layer 1 — generated parser entry points for each of the six
// gorgeous-backed grammars. The compile-time dependency is the
// document-owned parse/view surface, not the retired generated
// tape-view surface.
// ───────────────────────────────────────────────────────────────────

use ::bbnf::grammar::generated::bbnf::BbnfBootstrap;
use ::bbnf::grammar::generated::bnf::BnfParser;
use ::bbnf::grammar::generated::css_l4::CssL4Parser;
use ::bbnf::grammar::generated::ebnf::EbnfParser;
use ::bbnf::grammar::generated::google_sheets::GoogleSheetsParser;
use ::bbnf::grammar::generated::json::JsonParser;

// ───────────────────────────────────────────────────────────────────
// Layer 2 — runtime IR introspection: accessor-class classifier that
// mirrors `view/mod.rs::emit_typed_accessors` branch-for-branch.
// ───────────────────────────────────────────────────────────────────

/// Every accessor class the view emitter routes rules into. The set
/// is closed: every non-transparent rule resolves to exactly one
/// class via the same dispatcher the generator uses.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum AccessorClass {
    /// Aggregate payload — `.value() -> (T1, T2, ...)` tuple plus
    /// (when layout + named-type match) `.as_color()`.
    Aggregate,
    /// KV-pair payload — `.key()`, `.value() -> scalar`.
    KvPair,
    /// Leaf Span — `.text()`, `.value() -> &str`, `.as_span() -> &str`.
    /// Covers both explicitly `Span`-typed leaves AND leaves without a
    /// `TypeDesc` entry (the emitter defaults to `TypeDesc::Span`, so
    /// they share the same accessor surface).
    LeafSpan,
    /// Leaf scalar — `.text()`, `.value() -> T`, `.as_<T>() -> T`.
    LeafScalar,
    /// Seq body — `.child_N()` positional + named accessors.
    Seq,
    /// Alt body — `.as_<variant>()` / `.is_<variant>()` / `.chosen()`.
    /// When every branch is payload-eligible the dispatcher also
    /// emits a `<RuleName>Value` enum + `.value()`.
    Alt,
    /// Repeat body — `.iter()`, `.len()`, `.is_empty()`, `.get(i)`.
    Repeat,
}

/// Classify a non-transparent rule into its accessor class. Mirrors
/// the dispatcher in `view/mod.rs::emit_typed_accessors`.
fn classify_rule(rule: &IrRule, ir: &GrammarIR) -> AccessorClass {
    // Look up the rule's TypeDesc (if any).
    let type_desc = ir
        .types
        .iter()
        .find_map(|(id, ty)| (*id == rule.id).then_some(ty));

    // Aggregate-payload rules dispatch to the leaves emitter, further
    // split into KV-pair vs general aggregate.
    if let Some(_layout) = ir.payload_layouts.get(&rule.id) {
        let is_kv = type_desc.is_some_and(|td| match td {
            TypeDesc::Tuple(fields) => is_kv_pair_shape(fields),
            _ => false,
        });
        return if is_kv {
            AccessorClass::KvPair
        } else {
            AccessorClass::Aggregate
        };
    }

    // Peel through Map + OptionalWhitespace to find the meaningful
    // body shape (identical to `view/mod.rs::peel_body`).
    let body = peel_body(&rule.body);

    match body {
        IrNode::Seq(_) => AccessorClass::Seq,
        IrNode::Alt(_, _) => AccessorClass::Alt,
        IrNode::Repeat { .. } => AccessorClass::Repeat,
        _ => match type_desc {
            // Scalar payloads (F64, U8, Bool, etc.) go through
            // `emit_leaf_accessors` with `type_desc.is_scalar_payload()`
            // === true, producing `.value()` + `.as_<T>()`.
            Some(td) if td.is_scalar_payload() && !matches!(td, TypeDesc::Span) => {
                AccessorClass::LeafScalar
            }
            // Span-typed OR absent TypeDesc → the emitter falls back
            // to `TypeDesc::Span` (see `view/mod.rs` lines 442-445),
            // producing `.text()` + `.value() -> &str` + `.as_span()`.
            _ => AccessorClass::LeafSpan,
        },
    }
}

/// Peel through the same wrappers the emitter peels in
/// `view/mod.rs::peel_body`. Keeping this local ensures the audit
/// classifier and the generator peel in lockstep.
fn peel_body(node: &IrNode) -> &IrNode {
    match node {
        IrNode::Map { inner, .. } | IrNode::OptionalWhitespace(inner) => peel_body(inner),
        other => other,
    }
}

/// Compile a grammar file (with optional `@import` sub-modules) into
/// a VM-target `GrammarIR`. All audit grammars use `CompileTarget::Vm`
/// because the view emitter consumes the same IR.
fn compile_grammar_ir(rel_entry: &str) -> GrammarIR {
    let manifest = env!("CARGO_MANIFEST_DIR");
    let entry = PathBuf::from(manifest).join("../../").join(rel_entry);
    let request = CompileRequest {
        options: PipelineOptions::default(),
        target: CompileTarget::Vm,
    };
    let out = compile_paths_request(std::slice::from_ref(&entry), &request)
        .unwrap_or_else(|err| panic!("{rel_entry}: compile_paths_request failed: {err}"));
    match out {
        CompileOutput::Vm(ir) => ir,
        other => panic!("{rel_entry}: expected Vm output, got {other:?}"),
    }
}

/// Per-grammar coverage report: rule counts per accessor class.
#[derive(Debug, Default)]
struct CoverageReport {
    aggregate: usize,
    kv_pair: usize,
    leaf_span: usize,
    leaf_scalar: usize,
    seq: usize,
    alt: usize,
    repeat: usize,
    /// Number of skipped (transparent) rules — NOT part of the
    /// accessor surface.
    transparent_skipped: usize,
}

impl CoverageReport {
    fn record(&mut self, class: AccessorClass) {
        match class {
            AccessorClass::Aggregate => self.aggregate += 1,
            AccessorClass::KvPair => self.kv_pair += 1,
            AccessorClass::LeafSpan => self.leaf_span += 1,
            AccessorClass::LeafScalar => self.leaf_scalar += 1,
            AccessorClass::Seq => self.seq += 1,
            AccessorClass::Alt => self.alt += 1,
            AccessorClass::Repeat => self.repeat += 1,
        }
    }

    fn total_views_emitted(&self) -> usize {
        self.aggregate
            + self.kv_pair
            + self.leaf_span
            + self.leaf_scalar
            + self.seq
            + self.alt
            + self.repeat
    }
}

/// Audit a single grammar and return its coverage report.
fn audit_grammar(label: &str, rel_entry: &str) -> CoverageReport {
    let ir = compile_grammar_ir(rel_entry);
    let mut report = CoverageReport::default();

    for rule in &ir.rules {
        if rule.meta.is_transparent {
            report.transparent_skipped += 1;
            continue;
        }
        let class = classify_rule(rule, &ir);
        report.record(class);
    }

    eprintln!("─── {label} ({rel_entry}) ───");
    eprintln!(
        "  total rules:        {} ({} transparent, {} view-emitted)",
        ir.rules.len(),
        report.transparent_skipped,
        report.total_views_emitted()
    );
    eprintln!(
        "  Aggregate  (.value tuple / .as_color):  {}",
        report.aggregate
    );
    eprintln!(
        "  KvPair     (.key + .value):             {}",
        report.kv_pair
    );
    eprintln!(
        "  LeafSpan   (.text + .value -> &str):    {}",
        report.leaf_span
    );
    eprintln!(
        "  LeafScalar (.text + .value + .as_<T>):  {}",
        report.leaf_scalar
    );
    eprintln!("  Seq        (.child_N + named):          {}", report.seq);
    eprintln!("  Alt        (.as_<v> / .chosen / .value):{}", report.alt);
    eprintln!(
        "  Repeat     (.iter / .len / .get):       {}",
        report.repeat
    );
    eprintln!();

    // Every grammar must emit at least one view (non-empty surface).
    assert!(
        report.total_views_emitted() > 0,
        "{label}: zero views emitted — emitter regression",
    );

    report
}

// ───────────────────────────────────────────────────────────────────
// Per-grammar audit tests. Each compiles the grammar through the
// pipeline and classifies every non-transparent rule.
// ───────────────────────────────────────────────────────────────────

#[test]
fn json_accessor_surface() {
    let r = audit_grammar("JSON", "grammar/json/json.bbnf");
    // JSON has: `null` (leaf scalar u8), `bool` (Alt: true/false),
    // `number` (leaf scalar F64), `string` (leaf Span), `array`
    // (Seq with Repeat), `pair` (Seq → KV-pair via payload layout),
    // `object` (Seq with Repeat), `value` (Alt over 6 variants),
    // plus structural skip rules (`comma`, `colon`).
    assert!(
        r.kv_pair + r.aggregate + r.leaf_scalar + r.leaf_span >= 2,
        "JSON must emit at least two payload-bearing views (string, number, bool, pair)"
    );
}

#[test]
fn css_l4_accessor_surface() {
    let r = audit_grammar("CSS L4", "grammar/css/l4/stylesheet.bbnf");
    // CSS L4 is the richest grammar: keyword rules (U8 scalars),
    // dimension rules (Tuple(F64, U8) aggregates), colour-function
    // rules (5-field Aggregate + `.as_color()`), selectors (Alts),
    // declarations (Seqs), rule lists (Repeats).
    assert!(r.aggregate >= 1, "CSS L4 must have aggregate-payload rules");
    assert!(
        r.leaf_scalar >= 1,
        "CSS L4 must have scalar-leaf rules (u8 keywords)"
    );
    assert!(r.alt >= 1, "CSS L4 must have Alt rules");
    assert!(r.seq >= 1, "CSS L4 must have Seq rules");
    assert!(r.repeat >= 1, "CSS L4 must have Repeat rules");
}

#[test]
fn sheets_accessor_surface() {
    let r = audit_grammar("Sheets", "grammar/google-sheets/google-sheets.bbnf");
    // Sheets has: `number` (F64), `string` (Span), `boolean` (Bool),
    // `error_literal` (U8 Alt), cell refs (Seqs), ranges (Seqs),
    // functions (Seq + Repeat args), expressions (Alts).
    assert!(
        r.alt >= 1,
        "Sheets must have Alt rules (boolean, error_literal)"
    );
    assert!(
        r.leaf_scalar + r.leaf_span + r.aggregate + r.kv_pair >= 1,
        "Sheets must have payload-bearing views"
    );
    assert!(
        r.seq >= 1,
        "Sheets must have Seq rules (cell_ref, range_ref, function call)"
    );
}

#[test]
fn bbnf_accessor_surface() {
    let r = audit_grammar("BBNF", "grammar/bbnf/bbnf.bbnf");
    // BBNF self-hosted grammar: mostly structural. Must have Seqs
    // (rule = lhs = rhs), Alts (factor variants), Repeats
    // (grammar = rule*), and Span-leaf identifiers.
    assert!(r.seq >= 1, "BBNF must have Seq rules");
    assert!(r.alt >= 1, "BBNF must have Alt rules");
    assert!(r.repeat >= 1, "BBNF must have Repeat rules");
}

#[test]
fn ebnf_accessor_surface() {
    let r = audit_grammar("EBNF", "grammar/ebnf/ebnf.bbnf");
    // EBNF is purely structural: letter / digit / symbol are Alts,
    // identifier is Seq + Repeat, rule is Seq, grammar is Repeat.
    assert!(
        r.alt >= 1,
        "EBNF must have Alt rules (letter, digit, symbol)"
    );
    assert!(r.seq >= 1, "EBNF must have Seq rules (rule, identifier)");
    assert!(r.repeat >= 1, "EBNF must have Repeat rules (grammar)");
}

#[test]
fn bnf_accessor_surface() {
    let r = audit_grammar("BNF", "grammar/bnf/bnf.bbnf");
    // BNF is tiny — the `term = terminal | nonterminal` 2-branch Alt
    // is a single-use Alt that fuses into its call sites during
    // inlining, so the surviving rule set is (rule, expression,
    // alternation, nonterminal, terminal) with Repeat + Seq + leaf
    // shapes — no standalone Alt view. This is expected post-fuse
    // behaviour; the audit records the survivor classes.
    assert!(r.seq >= 1, "BNF must have Seq rules (rule, alternation)");
    assert!(r.repeat >= 1, "BNF must have Repeat rules (expression)");
    // At least one payload-bearing surface survives (KV-pair for
    // nonterminal, LeafSpan for terminal).
    assert!(
        r.kv_pair + r.leaf_span + r.leaf_scalar + r.aggregate >= 1,
        "BNF must have at least one payload-bearing view (terminal / nonterminal)"
    );
}

// ───────────────────────────────────────────────────────────────────
// Aggregate coverage gate — every emitter branch in the dispatcher
// must light up in at least one grammar in the six-grammar corpus.
// Regressions that silently drop an emitter branch show up here as
// coverage-zero deltas.
// ───────────────────────────────────────────────────────────────────

#[test]
fn every_accessor_class_has_nonzero_coverage() {
    let reports = [
        audit_grammar("JSON", "grammar/json/json.bbnf"),
        audit_grammar("CSS L4", "grammar/css/l4/stylesheet.bbnf"),
        audit_grammar("Sheets", "grammar/google-sheets/google-sheets.bbnf"),
        audit_grammar("BBNF", "grammar/bbnf/bbnf.bbnf"),
        audit_grammar("EBNF", "grammar/ebnf/ebnf.bbnf"),
        audit_grammar("BNF", "grammar/bnf/bnf.bbnf"),
    ];

    let sum = reports
        .iter()
        .fold(CoverageReport::default(), |mut acc, r| {
            acc.aggregate += r.aggregate;
            acc.kv_pair += r.kv_pair;
            acc.leaf_span += r.leaf_span;
            acc.leaf_scalar += r.leaf_scalar;
            acc.seq += r.seq;
            acc.alt += r.alt;
            acc.repeat += r.repeat;
            acc
        });

    eprintln!("╔═ Aggregate coverage across 6 grammars ═╗");
    eprintln!("  Aggregate  rules: {}", sum.aggregate);
    eprintln!("  KvPair     rules: {}", sum.kv_pair);
    eprintln!("  LeafSpan   rules: {}", sum.leaf_span);
    eprintln!("  LeafScalar rules: {}", sum.leaf_scalar);
    eprintln!("  Seq        rules: {}", sum.seq);
    eprintln!("  Alt        rules: {}", sum.alt);
    eprintln!("  Repeat     rules: {}", sum.repeat);
    eprintln!("  total views:      {}", sum.total_views_emitted());

    // Every major accessor class must have non-zero rule coverage
    // across the corpus. Aggregate + KvPair come from CSS L4 and
    // JSON; Leaf / Seq / Alt / Repeat are ubiquitous.
    assert!(
        sum.aggregate > 0,
        "Aggregate class uncovered — CSS L4 dimension / colour rules missing"
    );
    assert!(
        sum.kv_pair > 0,
        "KvPair class uncovered — no KV-pair-shaped rule in any grammar"
    );
    assert!(
        sum.leaf_scalar > 0,
        "LeafScalar class uncovered — no scalar-payload leaf rule"
    );
    assert!(
        sum.leaf_span > 0,
        "LeafSpan class uncovered — no Span-typed leaf rule"
    );
    assert!(
        sum.seq > 0,
        "Seq class uncovered — no Seq-body rule in any grammar"
    );
    assert!(
        sum.alt > 0,
        "Alt class uncovered — no Alt-body rule in any grammar"
    );
    assert!(
        sum.repeat > 0,
        "Repeat class uncovered — no Repeat-body rule in any grammar"
    );
}

// ───────────────────────────────────────────────────────────────────
// Layer 1 smoke — compile-time accessor calls through the derive-
// generated view types. Each call exercises a specific emitter branch
// against a concrete grammar + rule, so regressions that drop a
// per-branch accessor surface here as "no method named `<X>`" at
// build time.
//
// The tests do NOT need to run to gate the surface — compilation is
// the assertion. They DO run as inexpensive sanity checks that the
// parse + accessor chain works end-to-end.
// ───────────────────────────────────────────────────────────────────

#[test]
fn json_compile_time_accessors() {
    // AZ-I.W2-act.B1 — JSON crosses to the struct-direct path; the
    // grammar-emitted `JsonParser::parse` returns `JsonDocument<'_>`
    // and `doc.view()` yields the struct-tree `JsonView` (not the
    // tape-cursor `View`). The cursor-backed universal accessors are
    // replaced by the struct-tree surface: `kind()` / `is_*()` /
    // arena handle resolution / root borrow.
    let doc = JsonParser::parse("{\"ok\":true}").expect("JSON object parse");
    let view = doc.view();

    // Struct-tree accessors (always emitted on JsonView):
    let _: bbnf::runtime::JsonKind = view.kind();
    let _: bool = view.is_object();
    let _: bool = view.is_array();
    let _: bool = view.is_string();
    let _: bool = view.is_number();
    let _: bool = view.is_bool();
    let _: bool = view.is_null();
    let _: &bbnf::runtime::JsonValue<'_> = view.root();
    let _: &bbnf::runtime::JsonArena<'_> = view.arena();

    // The struct-tree shape is the post-flip evidence: an object root
    // resolves through the arena and exposes its typed discriminator.
    assert!(
        view.is_object(),
        "JsonParser::parse(\"{{\\\"ok\\\":true}}\") must yield an Object root",
    );
    assert_eq!(
        view.kind(),
        bbnf::runtime::JsonKind::Object,
        "JsonView::kind() must dispatch Object for an object root",
    );

    // Compile-time proof: the JsonDocument / JsonView types exist and
    // carry the expected shape. Per the W2-act.B1 substrate the
    // pre-W2-act `View` GAT is replaced for JSON; the struct-tree
    // types are referenced here so a future regression surfaces.
    fn _require_struct_types<'p>(
        _doc: bbnf::runtime::JsonDocument<'p>,
        _view: bbnf::runtime::JsonView<'_, 'p>,
        _kind: bbnf::runtime::JsonKind,
    ) {
    }
}

#[test]
fn css_l4_compile_time_accessors() {
    // AZ-I.W2-act.close B3 — CSS L4 crosses to the struct-direct path;
    // `CssL4Parser::parse` returns `CssDocument<'_>` and `doc.view()`
    // yields the struct-tree `CssView` (not the cursor-backed
    // `stylesheetView`). The cursor / rule_kind / input / span /
    // children surface from the pre-W2-act `Parsed::view()` is replaced
    // by the [`bbnf::runtime::RuntimeView`] trait surface, exercised
    // here in lockstep with the JSON / Sheets sibling tests.
    let src = "a { color: red; }";
    let doc = CssL4Parser::parse(src).expect("CSS L4 parse");
    let view = doc.view();

    // Struct-tree accessors on the CssView itself:
    let _: bbnf::runtime::CssDocumentKind = view.kind();
    let _: &bbnf::runtime::css_l4::StyleSheet = view.root();
    let _: &bbnf::runtime::css_l4::CssArena<'_> = view.arena();

    // RuntimeView trait surface — the uniform navigation API:
    use bbnf::runtime::RuntimeView;
    let _: bbnf::runtime::CssDocumentKind = RuntimeView::kind(&view);
    let _: Option<&str> = RuntimeView::span(&view);
    let _: &str = RuntimeView::input(&view);
    let _: Vec<_> = RuntimeView::children(&view).collect();

    // The struct-tree shape is the post-flip evidence: a non-empty
    // stylesheet root reports `StyleSheet`; an empty one reports
    // `Empty`. The discriminator must agree with the typed graph.
    assert_eq!(
        RuntimeView::kind(&view),
        bbnf::runtime::CssDocumentKind::StyleSheet,
        "CssL4Parser::parse(\"a {{ color: red; }}\") must yield a non-empty StyleSheet root",
    );

    fn _require_struct_types<'p>(
        _doc: bbnf::runtime::css_l4::CssDocument<'p>,
        _view: bbnf::runtime::css_l4::CssView<'_, 'p>,
        _kind: bbnf::runtime::CssDocumentKind,
    ) {
    }
}

#[test]
fn sheets_compile_time_accessors() {
    // AZ-I.W2-act.close B3 — Sheets crosses to the struct-direct path;
    // `GoogleSheetsParser::parse` returns `SheetsDocument<'_>` and
    // `doc.view()` yields the struct-tree `SheetsView`. The legacy
    // cursor surface is replaced by [`bbnf::runtime::RuntimeView`].
    let doc = GoogleSheetsParser::parse("=1+2").expect("Sheets parse");
    let view = doc.view();

    // Struct-tree accessors on the SheetsView itself:
    let _: bbnf::runtime::SheetsKind = view.kind();
    let _: &bbnf::runtime::SheetsValue<'_> = view.root();
    let _: &bbnf::runtime::SheetsArena<'_> = view.arena();

    // RuntimeView trait surface — uniform with JSON + CSS L4:
    use bbnf::runtime::RuntimeView;
    let _: bbnf::runtime::SheetsKind = RuntimeView::kind(&view);
    let _: Option<&str> = RuntimeView::span(&view);
    let _: &str = RuntimeView::input(&view);
    let _: Vec<_> = RuntimeView::children(&view).collect();

    fn _require_struct_types<'p>(
        _doc: bbnf::runtime::SheetsDocument<'p>,
        _view: bbnf::runtime::SheetsView<'_, 'p>,
        _kind: bbnf::runtime::SheetsKind,
    ) {
    }
}

#[test]
fn bbnf_compile_time_accessors() {
    use bbnf::runtime::RuntimeView;
    let src = "foo = \"a\" | \"b\" ;\n";
    let document = BbnfBootstrap::parse(src).expect("BBNF parse");
    let view = document.view();
    let _ = view.input();
    let _ = view.kind();
    let _ = RuntimeView::span(&view);
    let _ = view.compound_kind();
    let _: Vec<_> = view.children().collect();
}

#[test]
fn ebnf_compile_time_accessors() {
    // AZ-II.cutover.M Phase 3c — EBNF flipped to struct-direct; the
    // root view is the typed `EbnfView` document focus, not a tape
    // cursor. The compile-time proof references only the document-
    // owned runtime types so O3 can delete generated node-view and
    // `Root::View` aliases without breaking this test.
    let doc = EbnfParser::parse("letter = \"a\";").expect("EBNF parse");
    let view = doc.view();
    assert_eq!(view.input(), "letter = \"a\";");
    assert_eq!(view.kind(), bbnf::runtime::EbnfKind::Compound);
    assert_eq!(
        view.compound_kind(),
        Some(bbnf::runtime::EbnfCompoundKind::Grammar)
    );
    assert!(!bbnf::runtime::RuntimeView::children(&view)
        .collect::<Vec<_>>()
        .is_empty());

    fn _require_document_types<'p>(
        _doc: bbnf::runtime::EbnfDocument<'p>,
        _view: bbnf::runtime::EbnfView<'_, 'p>,
        _kind: bbnf::runtime::EbnfKind,
    ) {
    }
}

#[test]
fn bnf_compile_time_accessors() {
    // AZ-II.cutover.M Phase 3c — BNF flipped to struct-direct; same
    // surface narrowing as EBNF above.
    let doc = BnfParser::parse("<foo> ::= \"a\"\n").expect("BNF parse");
    let view = doc.view();
    assert_eq!(view.input(), "<foo> ::= \"a\"\n");
    assert_eq!(view.kind(), bbnf::runtime::BnfKind::Compound);
    assert_eq!(
        view.compound_kind(),
        Some(bbnf::runtime::BnfCompoundKind::Grammar)
    );
    assert!(!bbnf::runtime::RuntimeView::children(&view)
        .collect::<Vec<_>>()
        .is_empty());

    fn _require_document_types<'p>(
        _doc: bbnf::runtime::BnfDocument<'p>,
        _view: bbnf::runtime::BnfView<'_, 'p>,
        _kind: bbnf::runtime::BnfKind,
    ) {
    }
}

// ───────────────────────────────────────────────────────────────────
// Runtime accessor sanity — walk a parse and invoke `.rule_kind()`
// dispatch to confirm the enum is populated. Covers the
// generator's `RuleKind` enum + dispatch table emission.
// ───────────────────────────────────────────────────────────────────

#[test]
fn rule_kind_enum_dispatch_nonempty() {
    // AZ-I.W2-act.B1 — JSON's struct-direct path replaces the
    // cursor-backed rule_kind / children dispatch with the typed
    // `JsonValue` shape. `{"a":1}` parses to a JsonValue::Object root
    // whose pair slice resolves through the document arena; the
    // post-flip evidence is the typed shape (object handle resolves
    // to one pair) plus the kind discriminator.
    let doc = JsonParser::parse("{\"a\":1}").expect("JSON parse");
    let view = doc.view();
    assert_eq!(
        view.kind(),
        bbnf::runtime::JsonKind::Object,
        "JSON {{\"a\":1}} root must resolve as JsonKind::Object",
    );
    if let bbnf::runtime::JsonValue::Object(id) = doc.root {
        let pairs = doc.object(id);
        assert_eq!(pairs.len(), 1, "{{\"a\":1}} must resolve to one pair");
        assert_eq!(pairs[0].key, "a", "first pair key must be 'a'");
    } else {
        panic!("JSON {{\"a\":1}} root view kind reported Object but root is not Object");
    }
}

// ───────────────────────────────────────────────────────────────────
// O3.P1 — document-owned projection/accessor surface, per grammar.
// The retired projection-materializer/consumer slices belonged to
// generated tape views. StructDirect tests now prove each grammar's
// parse entry returns a document whose root and view accessors expose
// the typed value tree directly.
// ───────────────────────────────────────────────────────────────────

#[test]
fn struct_direct_document_projection_surface_per_grammar() {
    use bbnf::runtime::{
        BbnfCompoundKind, BbnfKind, BbnfValue, CssDocumentKind, CssRule, JsonKind, JsonValue,
        RuntimeView, SheetsKind, SheetsValue,
    };

    let json = JsonParser::parse("{\"a\":1,\"b\":true}").expect("JSON parse");
    let json_view = json.view();
    assert_eq!(json_view.kind(), JsonKind::Object);
    assert!(std::ptr::eq(json.to_value(), json.root()));
    let JsonValue::Object(object_id) = json.root() else {
        panic!("JsonParser root must be JsonValue::Object");
    };
    let json_pairs = json.object(*object_id);
    assert_eq!(json_pairs.len(), 2);
    assert_eq!(json_pairs[0].key, "a");

    let css = CssL4Parser::parse("a { color: red; }").expect("CSS L4 parse");
    let css_view = css.view();
    assert_eq!(RuntimeView::kind(&css_view), CssDocumentKind::StyleSheet);
    assert!(std::ptr::eq(css.to_value(), css.root()));
    let css_rules = css.rules(css.root().rules);
    let style_rule = css_rules
        .iter()
        .find_map(|rule| match rule {
            CssRule::Style(style_rule) => Some(*style_rule),
            _ => None,
        })
        .expect("CssL4Parser root must contain a style rule");
    let css_decls = css.decls(style_rule.declarations);
    assert!(
        !css_decls.is_empty(),
        "CssL4Parser style rule must expose document-owned declarations",
    );
    assert!(
        css.walk_values().count() > 0,
        "CssL4Parser document must expose typed values through document-owned walkers",
    );

    let sheets = GoogleSheetsParser::parse("=1+2").expect("Sheets parse");
    let sheets_view = sheets.view();
    assert_eq!(sheets_view.kind(), SheetsKind::Compound);
    assert!(std::ptr::eq(sheets.to_value(), sheets.root()));
    let SheetsValue::Compound(sheet_root) = sheets.root() else {
        panic!("GoogleSheetsParser root must be SheetsValue::Compound");
    };
    assert!(!sheets.compound(*sheet_root).children.is_empty());

    let bbnf = BbnfBootstrap::parse("foo = \"a\" | \"b\" ;\n").expect("BBNF parse");
    let bbnf_view = bbnf.view();
    assert_eq!(bbnf_view.kind(), BbnfKind::Compound);
    assert_eq!(bbnf_view.compound_kind(), Some(BbnfCompoundKind::Grammar));
    assert!(std::ptr::eq(bbnf.to_value(), bbnf.root()));
    let BbnfValue::Compound(bbnf_root) = bbnf.root() else {
        panic!("BbnfBootstrap root must be BbnfValue::Compound");
    };
    assert!(!bbnf.compound(*bbnf_root).children.is_empty());
}
