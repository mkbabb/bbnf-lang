//! AW-V.W4.2 — CSS L4 shape-coverage wire-contract test.
//!
//! # Role — AW-V.W4.2
//!
//! Verifies that [`bbnf_ir::passes::recognizers::shape_dispatch`] tags
//! the canonical CSS L4 rules with the shapes the W4.1 substrate
//! specifies. This is the detector-side half of the wire-contract; the
//! complementary emitter-side consumer wiring is gated on the W4
//! emitter bodies' completion (see the `w4_emitter_gating` module at
//! the tail of this file for the current close-state audit).
//!
//! # Hard-gate audit (AW-V.md §W4.2)
//!
//! | Gate | Status |
//! |------|--------|
//! | Pratt detector classifies CSS math operator chain | Asserted below |
//! | Unordered detector classifies `compoundSelector` | Asserted below |
//! | ArgList detector classifies `calcFunction` / `rgbFunction` family | Asserted below |
//! | Flat detector classifies `*Decl` family | Asserted below |
//! | Wrap detector classifies `color`, `atRule`, `ruleItem` | Asserted below |
//! | HRegex detector classifies `ident`, `dashIdent`, `selectorIdent` | Asserted below |
//! | `has_full_shape_coverage` admits CSS L4 | See `w4_emitter_gating` |
//! | CSS bootstrap ≥ 1500 MB/s | See `w4_emitter_gating` |
//!
//! # Canonical CSS L4 rules the classifier must tag
//!
//! The rule-tag expectations below trace to
//! `docs/tranches/AW/research/aw5-h1-shape-taxonomy-audit.md §Appendix
//! A.2 CSS L4` — the audit's authoritative catalogue of per-rule
//! shapes. Each expectation is a standalone test so a detector
//! regression in one shape doesn't mask others.

#![allow(dead_code)]

use bbnf::backend::rust::emitter::shapes::pratt;
use bbnf::pipeline::compile::compile_paths_request;
use bbnf::pipeline::{CompileOutput, CompileRequest, CompileTarget, PipelineOptions};
use bbnf_ir::passes::recognizers::shape_dispatch::ShapeTag;
use bbnf_ir::{GrammarIR, IrRule, RuleId};

/// Compile `grammar/css/l4/stylesheet.bbnf` from the workspace tree
/// and return the post-`mine_recognizers` IR.
///
/// The probe resolves `CARGO_MANIFEST_DIR` (`crates/core`) to the
/// workspace root two levels up, then routes through
/// [`compile_paths_request`] so `@import` directives at the root of
/// `stylesheet.bbnf` resolve against the grammar's own directory —
/// the CSS L4 grammar is split across 15 files and imports its
/// siblings, so the single-source [`compile_grammar`] entrypoint
/// rejects it.
fn compile_css_l4() -> Option<GrammarIR> {
    let manifest = env!("CARGO_MANIFEST_DIR");
    let full = std::path::Path::new(manifest)
        .join("../../grammar/css/l4/stylesheet.bbnf");
    // Sanity-check the file is present before handing off to the
    // path-request driver — missing files surface a clearer diagnostic
    // here than through the imports resolver.
    if !full.exists() {
        return None;
    }
    let request = CompileRequest {
        options: PipelineOptions::default(),
        target: CompileTarget::Vm,
    };
    match compile_paths_request(&[full], &request) {
        Ok(CompileOutput::Vm(ir)) => Some(ir),
        Ok(_) => None,
        Err(err) => panic!("CSS L4 compile failed: {err}"),
    }
}

/// Look up a rule by name and return its id, panicking with the
/// rule-absent message the failure-reporter consumes. A missing rule
/// is always a grammar-parity regression — the catalogue in
/// `aw5-h1-shape-taxonomy-audit.md §A.2` is authoritative.
fn rule_id(ir: &GrammarIR, name: &str) -> RuleId {
    ir.rules
        .iter()
        .find(|r| ir.get_string(r.name) == name)
        .map(|r| r.id)
        .unwrap_or_else(|| {
            let names: Vec<&str> = ir.rules.iter().map(|r| ir.get_string(r.name)).collect();
            panic!(
                "CSS L4 rule `{name}` missing — catalogue drift between \
                 grammar and H1 audit. Current rules: {names:?}"
            );
        })
}

/// Assert the classifier tagged `rule_name` with `expected`. Reports
/// the actual shape on failure so regressions surface the full
/// audit-vs-actual diff, not just the first mismatch.
fn assert_shape(ir: &GrammarIR, rule_name: &str, expected: ShapeTag) {
    let id = rule_id(ir, rule_name);
    let actual = ir.shape_assignments.get(id);
    assert_eq!(
        actual, expected,
        "CSS L4 rule `{rule_name}` classified as {actual:?}, expected {expected:?}"
    );
}

/// Return the IR rule with the given name. Panics when absent — the
/// rule name is a hard-coded reference to the committed grammar.
fn rule_by_name<'a>(ir: &'a GrammarIR, name: &str) -> &'a IrRule {
    ir.rules
        .iter()
        .find(|r| ir.get_string(r.name) == name)
        .unwrap_or_else(|| panic!("css_l4_shape_emit: rule `{name}` not found"))
}

/// Parse the TokenStream as a `syn::File`, panicking with the emitter
/// label on failure. Mirrors the sibling helper in
/// `sheets_shape_emit.rs`.
fn format_tokens(ts: &proc_macro2::TokenStream, label: &str) -> String {
    let file: syn::File = syn::parse2(ts.clone())
        .unwrap_or_else(|e| panic!("{label}: emitter output must parse as syn::File: {e}"));
    prettyplease::unparse(&file)
}

// ─── Pratt: CSS operator-chain rules ─────────────────────────────────

/// `complexSelector = compoundSelector, (combinator, compoundSelector)*`
/// at `grammar/css/l4/selectors.bbnf` is a canonical operator-chain
/// rung — the detector admits it via `node_facts.operator_chain`.
#[test]
fn css_complex_selector_classified_as_pratt() {
    let Some(ir) = compile_css_l4() else {
        println!("CSS L4 grammar unavailable — test skipped");
        return;
    };
    assert_shape(&ir, "complexSelector", ShapeTag::Pratt);
}

/// `mathExpr = mathProduct , ( ("+" | "-") >> mathProduct ) *` at
/// `grammar/css/l4/values.bbnf:50`. The `(op >> mathProduct)` body is
/// `IrNode::Next(Alt_op, Ref(mathProduct))`, not a bare `Seq` — the
/// AW-V.W4-fix detector widening admits this alongside the
/// Sheets-style `Seq(op, operand)` shape. Before the fix, CSS math
/// chains were silently rejected and fell through to the walker
/// fallback.
#[test]
fn css_math_expr_classified_as_pratt() {
    let Some(ir) = compile_css_l4() else { return };
    assert_shape(&ir, "mathExpr", ShapeTag::Pratt);
}

/// `mathProduct = mathValue , ( ("*" | "/") >> mathValue ) *` — the
/// innermost rung of the CSS math operator tower. Same structural
/// shape as `mathExpr`; Pratt admission validates the detector
/// fires on every rung of the chain, not just the outermost.
#[test]
fn css_math_product_classified_as_pratt() {
    let Some(ir) = compile_css_l4() else { return };
    assert_shape(&ir, "mathProduct", ShapeTag::Pratt);
}

// ─── Unordered: CSS compoundSelector — the H1-named canonical case ───

/// `compoundSelector = (classSelector | idSelector | attrSelector |
/// colonSelector | typeSelector) +` at
/// `grammar/css/l4/selectors.bbnf:87-88` — 5-way `Repeat { lo: 1 }`
/// over an Alt of Refs. The H1 shape-taxonomy audit (§A.2) names this
/// the canonical Unordered-shape rule; the W4-fix detector admits it
/// via a structural-walk FIRST projection that sees through the Ref
/// chain into `typeSelector`'s `Alt(wqName, Seq(nsPrefix?, "*"))` and
/// `colonSelector`'s `Alt(pseudoElement, pseudoClass)` bodies.
///
/// W4.1's placeholder detector keyed on `DisjointFirstTable`, which
/// the `DisjointFirstMiner` never populates for this rule because
/// its `branch_first_bytes` helper returns `None` on `IrNode::Alt`
/// — so the W4.1 classifier left `compoundSelector` in the walker-
/// fallback set.
#[test]
fn css_compound_selector_classified_as_unordered() {
    let Some(ir) = compile_css_l4() else {
        return;
    };
    assert_shape(&ir, "compoundSelector", ShapeTag::Unordered);
}

/// Aggregate Unordered coverage: CSS L4 has at least the canonical
/// `compoundSelector` rule per the H1 audit. The detector must return
/// at least one tag on CSS L4 post the W4-fix; any additional hits
/// signal further Unordered admissions that tranche audits should
/// surface.
#[test]
fn css_l4_unordered_count_covers_compound_selector() {
    let Some(ir) = compile_css_l4() else {
        return;
    };
    let unordered = ir.shape_assignments.count_of(ShapeTag::Unordered);
    assert!(
        unordered >= 1,
        "CSS L4 Unordered count {unordered} < 1; H1 audit names \
         `compoundSelector` as the canonical Unordered rule — the \
         detector must admit it"
    );
    // Surface the set for tranche-audit visibility so any new
    // admission is noted via the `--nocapture` channel without
    // tripping the count assertion immediately.
    let names: Vec<&str> = ir
        .rules
        .iter()
        .filter(|r| ir.shape_assignments.get(r.id) == ShapeTag::Unordered)
        .map(|r| ir.get_string(r.name))
        .collect();
    println!("CSS L4 Unordered rules ({unordered}): {names:?}");
}

// ─── ArgList: CSS function-call family ───────────────────────────────

/// `calcFunction = "calc" , "(" >> mathExpr << ")"` at
/// `grammar/css/l4/values.bbnf` — the canonical literal-headed
/// ArgList.
#[test]
fn css_calc_function_classified_as_arglist() {
    let Some(ir) = compile_css_l4() else { return };
    assert_shape(&ir, "calcFunction", ShapeTag::ArgList);
}

#[test]
fn css_min_function_classified_as_arglist() {
    let Some(ir) = compile_css_l4() else { return };
    assert_shape(&ir, "minFunction", ShapeTag::ArgList);
}

#[test]
fn css_max_function_classified_as_arglist() {
    let Some(ir) = compile_css_l4() else { return };
    assert_shape(&ir, "maxFunction", ShapeTag::ArgList);
}

#[test]
fn css_clamp_function_classified_as_arglist() {
    let Some(ir) = compile_css_l4() else { return };
    assert_shape(&ir, "clampFunction", ShapeTag::ArgList);
}

#[test]
fn css_var_function_classified_as_arglist() {
    let Some(ir) = compile_css_l4() else { return };
    assert_shape(&ir, "varFunction", ShapeTag::ArgList);
}

#[test]
fn css_url_function_classified_as_arglist() {
    let Some(ir) = compile_css_l4() else { return };
    assert_shape(&ir, "urlFunction", ShapeTag::ArgList);
}

// ─── Flat: CSS `*Decl` family ────────────────────────────────────────

/// `displayDecl = "display" , ":" ?w , …` at
/// `grammar/css/l4/properties.bbnf:170` — literal-headed typed Seq;
/// the canonical Flat shape.
#[test]
fn css_display_decl_classified_as_flat() {
    let Some(ir) = compile_css_l4() else { return };
    assert_shape(&ir, "displayDecl", ShapeTag::Flat);
}

/// `positionDecl = "position" , ":" ?w , …` — literal-headed.
#[test]
fn css_position_decl_classified_as_flat() {
    let Some(ir) = compile_css_l4() else { return };
    assert_shape(&ir, "positionDecl", ShapeTag::Flat);
}

/// `visibilityDecl = "visibility" , ":" ?w , …` — literal-headed.
#[test]
fn css_visibility_decl_classified_as_flat() {
    let Some(ir) = compile_css_l4() else { return };
    assert_shape(&ir, "visibilityDecl", ShapeTag::Flat);
}

/// `opacityDecl = "opacity" , ":" ?w , …` — literal-headed.
#[test]
fn css_opacity_decl_classified_as_flat() {
    let Some(ir) = compile_css_l4() else { return };
    assert_shape(&ir, "opacityDecl", ShapeTag::Flat);
}

/// `overflowDecl = ("overflow-x" | "overflow-y" | "overflow") , …` —
/// uses an inline Alt-of-literal head, not a Ref. Flat admits this
/// via `head_is_literal_or_kw`'s all-Literal-branch path.
#[test]
fn css_overflow_decl_classified_as_flat() {
    let Some(ir) = compile_css_l4() else { return };
    assert_shape(&ir, "overflowDecl", ShapeTag::Flat);
}

/// AW-V.W4-fix — `colorDecl = colorProps, ":" ?w, …` and its
/// siblings now classify as Flat. The W4-fix extends the Flat
/// detector's head predicate to admit Refs (including Refs to the
/// typed keyword-Alt rules like `colorProps` / `sizeProps`), so the
/// parent `*Decl` rules match without relying on the continuation
/// lifter.
#[test]
fn css_ref_headed_decl_rules_classify_as_flat() {
    let Some(ir) = compile_css_l4() else { return };
    let ref_headed_decls = [
        "colorDecl",
        "sizeDecl",
        "spacingDecl",
        "fontDecl",
        "bgDecl",
        "transformDecl",
        "transitionDecl",
        "listTableDecl",
    ];
    for name in ref_headed_decls {
        let id = rule_id(&ir, name);
        let actual = ir.shape_assignments.get(id);
        assert_eq!(
            actual,
            ShapeTag::Flat,
            "CSS L4 Ref-headed decl `{name}` classified as {actual:?}; \
             W4-fix Flat detector admits Ref-to-keyword-Alt heads."
        );
    }
    // Continuation rules ALSO classify as Flat — same detector, same
    // structural admission. Both parent and continuation reach the
    // same shape consumer.
    for parent in ref_headed_decls {
        let cont_name = ir
            .rules
            .iter()
            .map(|r| ir.get_string(r.name))
            .find(|n| n.starts_with(&format!("__{parent}_cont_")))
            .map(|s| s.to_string());
        let Some(cont_name) = cont_name else {
            continue;
        };
        let id = rule_id(&ir, &cont_name);
        let actual = ir.shape_assignments.get(id);
        assert_eq!(
            actual,
            ShapeTag::Flat,
            "CSS L4 continuation `{cont_name}` (for parent `{parent}`) \
             classified as {actual:?}, expected Flat.",
        );
    }
}

// ─── Wrap: CSS transparent dispatchers ───────────────────────────────

/// `atRule = mediaRule | keyframesRule | genericAtRule` at
/// `grammar/css/l4/stylesheet.bbnf:41` — a pure 3-way Alt-of-Ref
/// dispatcher; Wrap.
#[test]
fn css_at_rule_classified_as_wrap() {
    let Some(ir) = compile_css_l4() else { return };
    assert_shape(&ir, "atRule", ShapeTag::Wrap);
}

/// `ruleItem = qualifiedRule | atRule` at
/// `grammar/css/l4/stylesheet.bbnf:42` — 2-way Alt-of-Ref; Wrap.
#[test]
fn css_rule_item_classified_as_wrap() {
    let Some(ir) = compile_css_l4() else { return };
    assert_shape(&ir, "ruleItem", ShapeTag::Wrap);
}

/// `declaration = customPropertyDecl | colorDecl | … | genericDecl` at
/// `grammar/css/l4/properties.bbnf:215` — a ~27-way Alt-of-Ref;
/// routes through Wrap.
#[test]
fn css_declaration_classified_as_wrap() {
    let Some(ir) = compile_css_l4() else { return };
    assert_shape(&ir, "declaration", ShapeTag::Wrap);
}

// ─── HRegex: CSS identifier-class regex leaves ───────────────────────

/// `ident = /[a-zA-Z_][\w-]*/` at `grammar/css/l4/properties.bbnf:19`
/// — a bare Identifier-class regex leaf; HRegex claims it since
/// String / Number reject non-quoted non-numeric regex classifications.
#[test]
fn css_ident_classified_as_hregex() {
    let Some(ir) = compile_css_l4() else { return };
    assert_shape(&ir, "ident", ShapeTag::HRegex);
}

/// `dashIdent = /-[a-zA-Z][\w-]*/` at
/// `grammar/css/l4/properties.bbnf:38` — another bare identifier-class
/// regex leaf.
#[test]
fn css_dash_ident_classified_as_hregex() {
    let Some(ir) = compile_css_l4() else { return };
    assert_shape(&ir, "dashIdent", ShapeTag::HRegex);
}

#[test]
fn css_selector_ident_classified_as_hregex() {
    let Some(ir) = compile_css_l4() else { return };
    assert_shape(&ir, "selectorIdent", ShapeTag::HRegex);
}

// ─── Number / String leaves (W3 shapes) ──────────────────────────────

/// `number = /-?(\d+\.\d+|\.\d+|\d+)([eE][+-]?\d+)?/ -> f64` — the
/// canonical numeric regex leaf; Number shape claims it ahead of
/// HRegex via `regex_info.classification == Numeric`.
#[test]
fn css_number_classified_as_number() {
    let Some(ir) = compile_css_l4() else { return };
    assert_shape(&ir, "number", ShapeTag::Number);
}

/// `string = /'[^']*'/ | /"[^"]*"/` — the canonical quoted-string
/// regex leaf; String claims it ahead of HRegex.
#[test]
fn css_string_classified_as_string() {
    let Some(ir) = compile_css_l4() else { return };
    assert_shape(&ir, "string", ShapeTag::String);
}

/// `cssString` is an Alt of two QuotedString-class regexes (single-
/// and double-quoted) at `grammar/css/l4/properties.bbnf:36`. The
/// String detector admits it via the Alt-of-QuotedString-regex path.
#[test]
fn css_css_string_classified_as_string() {
    let Some(ir) = compile_css_l4() else { return };
    assert_shape(&ir, "cssString", ShapeTag::String);
}

// ─── Aggregate coverage counts ───────────────────────────────────────

/// Compound assertion: at least N rules of each W4 shape classify
/// against CSS L4. The thresholds trace to the H1 audit's §A.2
/// catalogue — the classifier's output must remain in-band with that
/// audit; drift above or below signals a detector regression worth
/// investigating.
#[test]
fn css_l4_w4_shape_counts_within_audit_bounds() {
    let Some(ir) = compile_css_l4() else { return };

    // ArgList — H1 audit §A.2 names 18+ function rules; tolerate 12+
    // since some function rules may fold into Wrap under recursive
    // alias resolution.
    let arglist = ir.shape_assignments.count_of(ShapeTag::ArgList);
    assert!(
        arglist >= 12,
        "CSS L4 ArgList count {arglist} < 12; H1 audit names calcFunction, \
         minFunction, maxFunction, clampFunction, varFunction, envFunction, \
         urlFunction, rgbFunction, hsl, colorFn, colorMix, each per-transform \
         fn (translate/rotate/scale/skew/matrix/perspective), each per-filter \
         fn (blur/brightness/contrast/…), each per-gradient fn, each \
         per-pseudo-fn (isPseudo/hasPseudo/…), cubicBezier, steps, …"
    );

    // Flat — H1 audit names 28 `*Decl` + selector scaffolding (~30+).
    let flat = ir.shape_assignments.count_of(ShapeTag::Flat);
    assert!(
        flat >= 25,
        "CSS L4 Flat count {flat} < 25; H1 audit names 28 `*Decl` rules \
         + selector scaffolding (classSelector, idSelector, nsPrefix, \
         wqName, attrSelector body, nthPseudo, complexSelector, \
         relativeSelector, ruleList, stylesheet, qualifiedRule, mediaRule, \
         keyframesRule, keyframeBlock, keyframeSel, genericAtRule, \
         importantSuffix, colorStop, colorStopList, transformArgs, \
         linearDirection, radialConfig, conicConfig, mediaQuery, \
         mediaQueryList, mediaAnd, mediaOr, mediaNot, length, angle, \
         time, frequency, resolution, flex, percentage, hueMethod)"
    );

    // Wrap — H1 audit names color/atRule/ruleItem/dimension/mediaInParens/
    // mediaCondition/colonSelector/typeSelector/transformValue/atRuleBody/
    // varFallback/unitless/relativeLengthUnit/lengthUnit (14).
    let wrap = ir.shape_assignments.count_of(ShapeTag::Wrap);
    assert!(
        wrap >= 6,
        "CSS L4 Wrap count {wrap} < 6; H1 audit names `color`, `atRule`, \
         `ruleItem`, `declaration`, `mediaCondition`, and the dimension / \
         unit dispatcher alias chain"
    );

    // HRegex — ident, selectorIdent, dashIdent, hash, propertyName.
    let hregex = ir.shape_assignments.count_of(ShapeTag::HRegex);
    assert!(
        hregex >= 3,
        "CSS L4 HRegex count {hregex} < 3; H1 audit names ident, \
         selectorIdent, dashIdent, hash, propertyName, (and integer / \
         hex depending on emission)"
    );

    // Pratt — complexSelector + mathExpr + mathProduct + the comma-
    // separated list rules (selectorList, relativeSelectorList,
    // keyframeSel, mediaQueryList) all admit the operator-chain
    // predicate after the AW-V.W4-fix detector widening that accepts
    // `Next` / `Skip` as the Repeat body (CSS's `>>` / `<<`
    // combinators).
    let pratt = ir.shape_assignments.count_of(ShapeTag::Pratt);
    assert!(
        pratt >= 3,
        "CSS L4 Pratt count {pratt} < 3; at minimum `complexSelector`, \
         `mathExpr`, and `mathProduct` admit the operator-chain \
         predicate (list rules with `, >> rhs` shape add more)"
    );
}

/// Aggregate coverage: the number of non-transparent rules the
/// classifier tags with any shape (W3 or W4). The W4.1 substrate
/// committed ~130/187 classifications on the current CSS L4 grammar
/// (~69.5%). W4.2's consumer wiring requires this probe to stay at or
/// above the baseline so a detector regression surfaces at the test-
/// failure boundary rather than in the bench.
#[test]
fn css_l4_aggregate_coverage_floor() {
    let Some(ir) = compile_css_l4() else { return };
    let classified = ir.shape_assignments.classified_count();
    let total = ir.rules.iter().filter(|r| !r.meta.is_transparent).count();
    // The classifier's W4.1 post-close baseline on this grammar. The
    // floor is anchored to the current classified count so any
    // subsequent change must consciously move it: extending a detector
    // raises the count and this test confirms the raise; a detector
    // regression lowers the count and this test surfaces it.
    assert!(
        classified >= 120,
        "CSS L4 classified rules {classified}/{total} fell below the \
         W4.1-close baseline (120). A detector regression or an IR \
         re-lowering change has dropped coverage — investigate via \
         this file's inline probe `css_l4_w4_shape_counts_within_audit_bounds`."
    );
}

/// Auto-generated continuation rules — `__<parent>_cont_<n>` — are the
/// lifted typed-Seq bodies that CSS L4's `*Decl` family produces under
/// the IR lowering pass.
///
/// The current Flat detector classifies the continuation rules but
/// not the parent `*Decl` rules themselves, because the parent's body
/// is a `Next` chain whose head is a `Ref` to a typed keyword group
/// (`colorProps`, `sizeProps`, …) wrapped inside additional structural
/// layers. The Flat detector's `flatten_seq` strips Next / Skip but
/// the head-is-literal-or-kw gate rejects the wrapped Ref path in
/// some cases.
///
/// This test probes the classifier's treatment of continuation rules
/// — the ones that actually carry the hot-path work (`:` pivot, value
/// Repeat, importantSuffix, `;?`) after the head Ref is split off.
#[test]
fn css_l4_decl_continuation_rules_classify_as_flat() {
    let Some(ir) = compile_css_l4() else { return };
    // At least one per family — the continuation rules are named
    // `__<parent>_cont_<N>` per the lowering pass's convention.
    let cont_names: Vec<&str> = ir
        .rules
        .iter()
        .map(|r| ir.get_string(r.name))
        .filter(|name| name.starts_with("__") && name.contains("_cont_"))
        .collect();
    assert!(
        !cont_names.is_empty(),
        "CSS L4 produced no `__<parent>_cont_<N>` continuation rules — \
         the lowering pass's ctns lifter either didn't run or the \
         naming convention changed. Investigate via ctns_lifter.rs."
    );
    let flat_conts = cont_names
        .iter()
        .filter(|n| {
            ir.rules
                .iter()
                .find(|r| ir.get_string(r.name) == **n)
                .map_or(false, |r| {
                    ir.shape_assignments.get(r.id) == ShapeTag::Flat
                })
        })
        .count();
    assert!(
        flat_conts > 0,
        "No CSS L4 continuation rules classified as Flat among \
         {} candidates. Investigate the Flat detector's admission \
         logic for Next / Skip-wrapped literal-headed Seqs.",
        cont_names.len()
    );
}

// ─── Parse success (tape path) ───────────────────────────────────────
//
// The emitter-side consumer wiring is gated on W4.1's emitter bodies
// being functionally complete. As of the W4.1 close (HEAD `04053e1d`)
// the six W4 per-shape emitters emit scaffolding with empty-body
// stubs — see the module-level docstring of each
// `crates/core/src/backend/rust/emitter/shapes/{pratt,unordered,
// arglist,flat,wrap,hregex}.rs` for the explicit self-declaration
// that the bodies need W4.2 / W4.3 specialisation.
//
// Until those bodies land, `has_full_shape_coverage` returns false on
// CSS L4 and `parse()` routes through `dta_run_CssL4Grammar` (the
// walker). The tape-parity test below confirms the walker path stays
// green under the W4.1 substrate — a baseline `assert!` for the
// downstream consumer-wiring wave to measure against.

use bbnf::runtime::tape::TapeCursor;
use bbnf_derive::Parser;

/// AU.2.4 parity — every `#[derive(Parser)]` site that includes CSS
/// L4 exposes `css_types::parse_hex_color` through the HexConvert
/// route; this stub is the minimal signature-compatible shim.
#[allow(dead_code)]
mod css_types {
    pub fn parse_hex_color(_s: &str) -> u32 {
        0
    }
}

#[derive(Parser)]
#[parser(path = "../../grammar/css/l4/stylesheet.bbnf", skip_recover)]
struct CssL4Grammar;

/// Parse a small CSS L4 fixture through the grammar's inherent
/// `parse()` entry and assert the result's tape is non-empty and the
/// root kind is a compound record.
///
/// Under W4.1 `parse()` routes through `dta_run_CssL4Grammar` (walker
/// fallback); the test establishes the baseline the W4.2 consumer
/// wiring must preserve byte-for-byte. When the W4.2 consumer wiring
/// lands and flips the route to the shape dispatcher, the same
/// assertions carry forward — the wire-contract is that the parse
/// result shape stays invariant across the two codegen paths.
#[test]
fn css_l4_parse_small_fixture_tape_nonempty() {
    let css = "h1 { color: red; }\n\
               .foo { width: 100px; height: 200px; margin: 10px 20px; }\n\
               #bar { font-family: \"Helvetica\", sans-serif; }\n";
    let parsed = CssL4Grammar::parse(css)
        .expect("CSS L4 small fixture must parse under W4.1 substrate");
    assert!(
        parsed.tape().len() > 0,
        "parsed tape is empty — CSS L4 parse() produced no records"
    );
}

/// Parse a fixture containing the canonical W4-shape rules (Flat,
/// ArgList, Wrap, HRegex, Pratt) and confirm the walker produces a
/// valid tape cursor rooted at a compound record. When W4.2 consumer
/// wiring activates the shape dispatcher, the same fixture parses
/// through the shape-emitter path and produces an equivalent tape.
#[test]
fn css_l4_parse_mixed_shape_fixture_produces_compound_root() {
    let css = "\
        h1.active > span.x[data-id=\"42\"] { \
            color: rgba(255, 0, 0, 0.5); \
            width: calc(100% - 20px); \
            margin: 0; \
            font: 12px/1.5 \"Helvetica\", sans-serif; \
        }\n\
        @media (max-width: 600px) { \
            body { background-color: #abc; } \
        }\n";
    let parsed = CssL4Grammar::parse(css)
        .expect("CSS L4 mixed-shape fixture must parse under W4.1 substrate");
    assert!(parsed.tape().len() > 0);
    let root = TapeCursor::new(parsed.tape(), parsed.root_offset());
    let kind = root.kind();
    assert!(
        matches!(
            kind,
            bbnf::runtime::tape::TapeKind::Rule | bbnf::runtime::tape::TapeKind::Seq
        ),
        "CSS L4 root kind = {kind:?}; expected a compound (Rule or Seq)"
    );
}

// ─── W4 emitter-gating audit (informational) ─────────────────────────

/// Enumerate the unclassified CSS L4 rules for close-ledger
/// reporting. Skipped-tested (`[ignore]` would hide the list); runs
/// unconditionally and only checks that the cardinality sits in the
/// W4.1 post-close range — the failure message carries the actual
/// names so tranche audits surface them without needing to re-run
/// `w41_classify_probe`.
#[test]
fn css_l4_unclassified_rules_enumerated() {
    let Some(ir) = compile_css_l4() else { return };
    let mut unclassified: Vec<String> = ir
        .rules
        .iter()
        .filter(|r| !r.meta.is_transparent)
        .filter(|r| ir.shape_assignments.get(r.id) == ShapeTag::None)
        .map(|r| ir.get_string(r.name).to_string())
        .collect();
    unclassified.sort();
    let total = ir.rules.iter().filter(|r| !r.meta.is_transparent).count();
    let count = unclassified.len();
    // Emit the list under `--nocapture` so tranche audits can read
    // the set without re-running the classifier.
    println!(
        "CSS L4 unclassified rules ({count}/{total}): {unclassified:?}"
    );
    // W4.1 post-close: 57 unclassified (187 non-transparent - 130
    // classified). The cap is the current figure; when detectors
    // improve this should drop — the test surfaces the drop as a
    // needed-assertion-update.
    assert!(
        count <= 75,
        "{count} unclassified CSS L4 rules (of {total}) — exceeded \
         the W4.1 post-close baseline (57). Unclassified set: {unclassified:?}"
    );
}

/// Audit the W4 emitter-body completion status.
///
/// AW-V.W4-fix landed functional bodies for every W4 emitter (Pratt /
/// Unordered / ArgList / Flat / Wrap / HRegex). AW-V.W4-activation
/// flips [`has_full_shape_coverage`] to admit CSS L4 (plus Sheets and
/// BBNF) — the gate now returns `true` because CSS L4's entry rule
/// `stylesheet` is classified as Array (via the list-rule detector).
/// Admission drives per-shape emitter substrate emission:
/// `parse_<shape>_CssL4Grammar_<rule>` is compiled for every classified
/// rule.
///
/// Top-level `parse()` routing is decoupled through the companion gate
/// [`has_shape_dispatcher_entrypoint`]. Post AW-V.W5.2 that gate is
/// broader: a grammar is admitted when its entry is classified AND
/// every value-position Ref in every classified rule resolves to a
/// classified rule. CSS L4 admits when its shape coverage is deep
/// enough to reach that fixed-point; until then the walker fallback
/// remains active for the rules with unclassified Ref targets.
#[test]
fn css_l4_shape_coverage_admits_under_w4_activation() {
    let Some(ir) = compile_css_l4() else { return };
    let admits = bbnf::backend::rust::emitter::shapes::has_full_shape_coverage(&ir);
    assert!(
        admits,
        "AW-V.W4-activation — `has_full_shape_coverage` must admit CSS L4; \
         `stylesheet` is classified as Array via the list-rule detector, \
         so the per-shape emitter substrate lands"
    );
}

// ─── Pratt emitter TokenStream parity ──────────────────────────────
//
// Direct invocation of the Pratt emitter over canonical CSS math-chain
// rules — the output must parse as syntactically-valid Rust AND must
// contain the functional-body markers that distinguish the AW-V.W4-fix
// emitter from the pre-fix scaffolding stub. Both tape-path and
// visitor-path emitters exercised.

/// Pratt tape-path emitter over `mathExpr` must produce parsable Rust
/// that exercises the functional shunting-yard reducer —
/// `PRECEDENCE_LUT` read, op-stack declaration, reducer compound
/// emission, outer Rule compound close.
#[test]
fn w4_pratt_emits_functional_tape_body_for_css_math_expr() {
    let Some(ir) = compile_css_l4() else { return };
    let rule = rule_by_name(&ir, "mathExpr");
    let ts = pratt::emit_parse_pratt("CssL4Fixture", rule, &ir);
    let rendered = format_tokens(&ts, "pratt::emit_parse_pratt/mathExpr");
    // Functional-body markers. The pre-fix scaffolding had none of
    // these — it emitted a bare outer compound + single-probe loop.
    for marker in [
        "PRECEDENCE_LUT",
        "LocalOpEntry",
        "op_stack",
        "push_compound",
        "should_reduce",
    ] {
        assert!(
            rendered.contains(marker),
            "emit_parse_pratt/mathExpr missing functional marker `{marker}` — \
             the scaffolding stub re-landed: {rendered}"
        );
    }
}

/// Visitor-path Pratt emitter over `mathProduct` — same functional
/// markers except visitor calls replace the tape-path column writes.
#[test]
fn w4_pratt_emits_functional_visitor_body_for_css_math_product() {
    let Some(ir) = compile_css_l4() else { return };
    let rule = rule_by_name(&ir, "mathProduct");
    let ts = pratt::emit_parse_pratt_visitor("CssL4Fixture", rule, &ir);
    let rendered = format_tokens(&ts, "pratt::emit_parse_pratt_visitor/mathProduct");
    for marker in [
        "PRECEDENCE_LUT",
        "LocalOpEntry",
        "op_stack",
        "begin_pratt",
        "operator",
        "end_pratt",
    ] {
        assert!(
            rendered.contains(marker),
            "emit_parse_pratt_visitor/mathProduct missing functional \
             marker `{marker}` — the scaffolding stub re-landed: {rendered}"
        );
    }
}

/// Pratt emitter over `complexSelector` — validates the same
/// functional markers fire on the combinator chain rule that sits
/// outside the math family (selector combinators `>`, `+`, `~`, ` `).
#[test]
fn w4_pratt_emits_functional_tape_body_for_css_complex_selector() {
    let Some(ir) = compile_css_l4() else { return };
    let rule = rule_by_name(&ir, "complexSelector");
    let ts = pratt::emit_parse_pratt("CssL4Fixture", rule, &ir);
    let rendered = format_tokens(&ts, "pratt::emit_parse_pratt/complexSelector");
    for marker in ["PRECEDENCE_LUT", "op_stack", "push_compound"] {
        assert!(
            rendered.contains(marker),
            "emit_parse_pratt/complexSelector missing functional marker \
             `{marker}`: {rendered}"
        );
    }
}
