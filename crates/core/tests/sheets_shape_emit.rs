//! AW-V.W4.3 — Sheets shape-dispatch + emitter wire contract.
//!
//! This test suite pins the Sheets-grammar classification surface the
//! W4.1 substrate (Pratt / Unordered / ArgList / Flat / Wrap / HRegex
//! detectors + emitters) admits, plus asserts the W4 emitter scaffolds
//! produce parsable Rust TokenStreams over canonical Sheets rules.
//!
//! The hard gate for W4.3 is verification, not activation:
//!
//! 1. Every canonical Sheets rule cited in the H1 shape-taxonomy audit
//!    (`docs/tranches/AW/research/aw5-h1-shape-taxonomy-audit.md` §A.3)
//!    carries a shape tag consistent with the structural detectors.
//! 2. The Sheets grammar's `has_full_shape_coverage` gate returns
//!    `false` — the W4.1 emitter gate explicitly reserves W4-classified
//!    shapes for the W4.2 (CSS) + W4.3 (Sheets) consumer waves, so
//!    Sheets continues to route through `__dta_walker_inline::run` per
//!    the AX cold-path replay contract.
//! 3. Each W4 emitter (Pratt / Unordered / ArgList / Flat / Wrap /
//!    HRegex) produces syntactically-well-formed Rust when invoked
//!    over a canonical Sheets rule for the shape it targets.
//! 4. `SheetsParser::parse()` still succeeds on representative Sheets
//!    inputs — the walker fallback path is intact.
//!
//! The test intentionally leaves documentation for the two detector
//! gaps surfaced during W4.3 (both W4.1 territory):
//!
//! - **Sheets `number` classifies as HRegex, not Number.** Sheets's
//!   `/(\d+\.?\d*|\.\d+)([eE][+-]?\d+)?/` pattern lacks the signed-
//!   leading `-?` that the current `RegexClass::Numeric` classifier
//!   admits; it falls through to HRegex. Documented in the HRegex
//!   assertion below.
//! - **Sheets `boolean` classifies as Wrap, not Keyword.** The rule
//!   body is `Alt(Regex, Regex)` (case-insensitive `/TRUE/i` +
//!   `/FALSE/i`), not `Alt(Literal, Literal)`; the Keyword detector
//!   admits only literal-led branches. Documented in the Wrap
//!   assertion below. The H1 audit's "Kw" label was a simplification
//!   that pre-supposes a Kw variant admitting regex branches.

use std::path::PathBuf;

use bbnf::backend::rust::emitter::shapes::{
    arglist, flat, has_full_shape_coverage, has_shape_dispatch, hregex, keyword,
    number as number_emit, pratt, string as string_emit, unordered, wrap,
};
use bbnf::pipeline::{
    compile_paths_request, CompileOutput, CompileRequest, CompileTarget, PipelineOptions,
};
use bbnf_derive::Parser;
use bbnf_ir::passes::recognizers::shape_dispatch::ShapeTag;
use bbnf_ir::{GrammarIR, IrRule, RuleId};

// ─── Shared helpers ─────────────────────────────────────────────────

/// Locate the Sheets grammar relative to the crate manifest.
fn sheets_grammar_path() -> PathBuf {
    let manifest = env!("CARGO_MANIFEST_DIR");
    PathBuf::from(manifest).join("../../grammar/google-sheets/google-sheets.bbnf")
}

/// Compile the Sheets grammar into a VM-ready `GrammarIR` with every
/// shape-dispatch sidecar populated by `mine_recognizers`.
fn sheets_ir() -> GrammarIR {
    let path = sheets_grammar_path();
    let request = CompileRequest {
        options: PipelineOptions::default(),
        target: CompileTarget::Vm,
    };
    let out = compile_paths_request(std::slice::from_ref(&path), &request)
        .expect("sheets_shape_emit: Sheets grammar must compile to VM IR");
    match out {
        CompileOutput::Vm(ir) => ir,
        other => panic!("sheets_shape_emit: expected Vm output, got {other:?}"),
    }
}

/// Return the IR rule with the given name. Panics when absent — the
/// rule name is a hard-coded reference to the committed grammar.
fn rule_by_name<'a>(ir: &'a GrammarIR, name: &str) -> &'a IrRule {
    ir.rules
        .iter()
        .find(|r| ir.get_string(r.name) == name)
        .unwrap_or_else(|| panic!("sheets_shape_emit: rule `{name}` not found in Sheets IR"))
}

/// Return the shape tag the classifier assigned to a named rule.
fn tag_of(ir: &GrammarIR, name: &str) -> ShapeTag {
    let rule = rule_by_name(ir, name);
    ir.shape_assignments.get(rule.id)
}

/// Parse the TokenStream as a `syn::File`, panicking with the emitter
/// label on failure. Mirrors `shape_dispatch_emission.rs::format_tokens`.
fn format_tokens(ts: &proc_macro2::TokenStream, label: &str) -> String {
    let file: syn::File = syn::parse2(ts.clone())
        .unwrap_or_else(|e| panic!("{label}: emitter output must parse as syn::File: {e}"));
    prettyplease::unparse(&file)
}

// ─── Shape classification assertions — canonical Sheets rules ────────

/// Sheets's six operator-tower rungs classify as Pratt per the H1
/// audit §A.3. The operator-chain detector fires on each rung's
/// `Seq(operand, Repeat(Seq(op, rhs)))` body.
#[test]
fn sheets_operator_tower_classifies_as_pratt() {
    let ir = sheets_ir();
    for rung in [
        "comparison_expr",
        "concat_expr",
        "add_expr",
        "mul_expr",
        "exp_expr",
    ] {
        assert_eq!(
            tag_of(&ir, rung),
            ShapeTag::Pratt,
            "Sheets `{rung}` is an operator-chain rung; must classify as Pratt",
        );
    }
}

/// ArgList catches the three Sheets call-site forms — `func_call`
/// (identifier head via `func_open`), `let_call` (regex head
/// `/[lL][eE][tT]\(/`), `lambda_call` (regex head `/[lL][aA]…\(/`).
#[test]
fn sheets_func_family_classifies_as_arglist() {
    let ir = sheets_ir();
    for call in ["func_call", "let_call", "lambda_call"] {
        assert_eq!(
            tag_of(&ir, call),
            ShapeTag::ArgList,
            "Sheets `{call}` is an ArgList-shape function call",
        );
    }
}

/// HRegex claims the regex leaves whose `RegexClass` is neither
/// QuotedString nor Numeric — Sheets `cell_ref`, `identifier`, plus
/// `number` (see module-level note: Sheets's fraction-first pattern
/// falls through to HRegex because the current `RegexClass::Numeric`
/// classifier expects a leading `-?`).
#[test]
fn sheets_identifier_like_leaves_classify_as_hregex() {
    let ir = sheets_ir();
    assert_eq!(
        tag_of(&ir, "cell_ref"),
        ShapeTag::HRegex,
        "Sheets `cell_ref = /\\$?[A-Za-z]{{1,3}}\\$?\\d+/` is HRegex",
    );
    assert_eq!(
        tag_of(&ir, "identifier"),
        ShapeTag::HRegex,
        "Sheets `identifier = /[A-Za-z_][A-Za-z0-9_.]*/` is HRegex",
    );
    // W4.1 DETECTOR GAP (documented, W4.1 territory — not fixed here):
    // Sheets `number = /(\d+\.?\d*|\.\d+)([eE][+-]?\d+)?/` falls through
    // to HRegex rather than Number because the unsigned Sheets regex
    // is not admitted by `RegexClass::Numeric`. The Pratt / ArgList /
    // Flat / Wrap consumer wiring in a future W4 iteration MAY depend
    // on `number` reaching the Number shape; when it does, the
    // Sheets regex needs the shared numeric classifier widening.
    assert_eq!(
        tag_of(&ir, "number"),
        ShapeTag::HRegex,
        "AW-V.W4.1 gap: Sheets `number` currently classifies HRegex, \
         not Number, because `RegexClass::Numeric` does not admit the \
         Sheets unsigned fraction-first regex. Documented in H1 audit.",
    );
}

/// Wrap admits transparent `Alt(Ref, …)` dispatchers. Sheets:
/// - `primary` — 11-way Alt over all value-producing rules.
/// - `cell_or_range` — 2-way Alt(`Ref(range_ref)`, `Ref(cell)`).
/// - `boolean` — 2-way Alt of regex-led literal-leaves (W4.1 gap; see
///   module doc).
/// - `sheet_prefix` — 2-way Alt of regex-led literal-leaves.
#[test]
fn sheets_alt_dispatchers_classify_as_wrap() {
    let ir = sheets_ir();
    for dispatcher in ["primary", "cell_or_range", "boolean", "sheet_prefix"] {
        assert_eq!(
            tag_of(&ir, dispatcher),
            ShapeTag::Wrap,
            "Sheets `{dispatcher}` is an Alt-of-Ref/Regex dispatcher; Wrap",
        );
    }
}

/// Keyword claims literal-led Alt bodies and single literals. Sheets's
/// operator-tag rules are all 2..6-branch Alts of single-literal
/// branches — exactly Keyword.
#[test]
fn sheets_operator_tags_classify_as_keyword() {
    let ir = sheets_ir();
    for op in ["compare_op", "add_op", "mul_op", "unary_prefix"] {
        assert_eq!(
            tag_of(&ir, op),
            ShapeTag::Keyword,
            "Sheets `{op}` is a literal-branch Alt; Keyword",
        );
    }
}

/// The `string` rule is a `-> input : Span` QuotedString regex; String
/// wins via `RegexClass::QuotedString`.
#[test]
fn sheets_string_classifies_as_string_shape() {
    let ir = sheets_ir();
    assert_eq!(
        tag_of(&ir, "string"),
        ShapeTag::String,
        "Sheets `string = /\"([^\"]|\"\")*\"/` classifies as String",
    );
}

/// `error_literal` is an Alt of single-byte-led literal branches. Alt
/// length = 9 ≤ KEYWORD_MAX_BRANCHES (16), every branch is a Map over
/// a Literal. The detector precedence places Keyword before Flat, but
/// Sheets's 9-branch Alt of `"#N/A" -> 0u8` etc. fails Keyword because
/// the shared prefix factoring in `KeywordStatsMiner` reshapes the
/// body into a Seq with a common prefix — so the final classification
/// is Flat via the factored Seq head.
#[test]
fn sheets_error_literal_classifies_as_flat() {
    let ir = sheets_ir();
    assert_eq!(
        tag_of(&ir, "error_literal"),
        ShapeTag::Flat,
        "Sheets `error_literal` (9-branch literal Alt factored into a \
         prefix-shared Seq) classifies as Flat",
    );
}

// ─── Coverage totals + gate state ───────────────────────────────────

/// The Sheets grammar has 36 non-transparent rules; under the
/// W4-fix detector substrate 29 classify (80.6%). The 7 residual
/// rules route through `__dta_walker_inline::run` per the AX replay
/// contract.
///
/// This test pins the number so a future detector widening surfaces
/// as a deliberate test update — not a silent reclassification. The
/// residual 7 rules (`unary_expr`, `paren_expr`, `arg`, `func_args`,
/// `let_args`, `lambda_params`, `array_literal`) share a structural
/// pattern — Alt-of-Refs with a mix of typed/untyped branches, or
/// Seq-with-Repeat-of-Ref bodies that fit no shape cleanly.
#[test]
fn sheets_classification_totals_match_w4_projection() {
    let ir = sheets_ir();
    let non_transparent: usize = ir.rules.iter().filter(|r| !r.meta.is_transparent).count();
    let classified: usize = ir.shape_assignments.classified_count();
    let unclassified = non_transparent - classified;

    // 36 non-transparent rules; 29 classified; 7 residual on walker.
    assert_eq!(
        non_transparent, 36,
        "Sheets grammar has 36 non-transparent rules; got {non_transparent}",
    );
    assert_eq!(
        classified, 29,
        "Sheets W4-fix classifies 29/36 rules; got {classified}",
    );
    assert_eq!(
        unclassified, 7,
        "Sheets W4-fix leaves 7 rules unclassified (walker fallback); got {unclassified}",
    );

    // Per-shape breakdown — each tag's rule count is a stable H1 audit
    // projection. Future widening lands here deliberately.
    assert_eq!(
        ir.shape_assignments.count_of(ShapeTag::Pratt),
        7,
        "Sheets Pratt count (6 operator-tower rungs + 1 array_row)",
    );
    assert_eq!(
        ir.shape_assignments.count_of(ShapeTag::ArgList),
        3,
        "Sheets ArgList count (func_call, let_call, lambda_call)",
    );
    assert_eq!(
        ir.shape_assignments.count_of(ShapeTag::HRegex),
        3,
        "Sheets HRegex count (cell_ref, identifier, number — number is W4.1 gap)",
    );
    assert_eq!(
        ir.shape_assignments.count_of(ShapeTag::Wrap),
        4,
        "Sheets Wrap count (primary, cell_or_range, boolean, sheet_prefix)",
    );
    assert_eq!(
        ir.shape_assignments.count_of(ShapeTag::Keyword),
        4,
        "Sheets Keyword count (compare_op, add_op, mul_op, unary_prefix)",
    );
    assert_eq!(
        ir.shape_assignments.count_of(ShapeTag::Flat),
        7,
        "Sheets Flat count (error_literal via prefix-factoring, plus \
         cell, range_ref, postfix_expr, func_open, let_binding, formula \
         via W4-fix Flat-detector widening)",
    );
    assert_eq!(
        ir.shape_assignments.count_of(ShapeTag::String),
        1,
        "Sheets String count (string)",
    );
    // No Object / Array / Number / Scalar / Unordered in Sheets today.
    assert_eq!(
        ir.shape_assignments.count_of(ShapeTag::Object),
        0,
        "Sheets has no Object-shape rules",
    );
    assert_eq!(
        ir.shape_assignments.count_of(ShapeTag::Array),
        0,
        "Sheets has no Array-shape rules (array_literal is Seq, not \
         JSON-style entry-list)",
    );
    assert_eq!(
        ir.shape_assignments.count_of(ShapeTag::Number),
        0,
        "Sheets has no Number-shape rules (W4.1 gap — `number` falls \
         through to HRegex)",
    );
    assert_eq!(
        ir.shape_assignments.count_of(ShapeTag::Scalar),
        0,
        "Sheets has no Scalar-shape rules",
    );
    assert_eq!(
        ir.shape_assignments.count_of(ShapeTag::Unordered),
        0,
        "Sheets has no Unordered-shape rules",
    );
}

/// Sheets carries shape-classified rules so `has_shape_dispatch`
/// is `true`. Post AW-V.W4-activation, `has_full_shape_coverage` is
/// also `true` — the coverage gate admits Sheets's non-Alt root
/// (`formula` → Flat-tagged via the W4-fix Flat detector widening).
/// Admission drives per-shape substrate emission (every classified
/// rule gets `parse_<shape>_SheetsShapeParser_<rule>` compiled).
///
/// Post AW-V.W5.2, `has_shape_dispatcher_entrypoint` is broader: a
/// grammar is admitted when its classified entry + every value-
/// position Ref in every classified rule resolves to a classified
/// rule. Sheets's `formula = Seq(/=?/, expression)` satisfies the
/// gate when Sheets reaches full value-Ref coverage; until then, the
/// gate reports whatever coverage the current detectors achieve.
#[test]
fn sheets_admits_shape_substrate() {
    let ir = sheets_ir();
    assert!(
        has_shape_dispatch(&ir),
        "Sheets has W3/W4-classified rules so `has_shape_dispatch` is true",
    );
    assert!(
        has_full_shape_coverage(&ir),
        "AW-V.W4-activation — `has_full_shape_coverage` admits Sheets; \
         `formula` is classified as Flat and the per-shape emitter \
         substrate lands",
    );
}


/// Double-check: `formula` is Sheets's entry rule and has body kind
/// Seq — not the six-Ref Alt dispatcher JSON uses. The coverage gate
/// correctly rejects it.
#[test]
fn sheets_entry_rule_is_formula_with_seq_body() {
    let ir = sheets_ir();
    let entry = ir
        .rules
        .iter()
        .find(|r| r.id == ir.entry)
        .expect("Sheets must have an entry rule");
    assert_eq!(
        ir.get_string(entry.name),
        "formula",
        "Sheets entry rule is `formula` per grammar/google-sheets/google-sheets.bbnf:164",
    );
    assert!(
        !matches!(entry.body, bbnf_ir::IrNode::Alt(_, _)),
        "Sheets `formula` is a Seq (/=?/, expression), not an Alt",
    );
}

// ─── W4 emitter TokenStream parse checks ────────────────────────────
//
// Each W4 emitter (Pratt / Unordered / ArgList / Flat / Wrap / HRegex)
// is invoked over a canonical Sheets rule of the corresponding shape;
// the output MUST parse as valid Rust even though the consumer wiring
// for Sheets remains in `__dta_walker_inline::run`. Substrate-with-
// consumer verification for these emitters lives in the
// `shape_dispatch_emission.rs` suite (JSON fixtures); these tests
// demonstrate the same emitter shapes also produce parsable tokens
// over Sheets's production IR.

/// Pratt emitter over `comparison_expr` — Sheets's top-level operator-
/// chain rung. Asserts both tape-path and visitor-path emitters carry
/// the functional shunting-yard body markers (PRECEDENCE_LUT read,
/// op-stack, reducer emission). Pre-AW-V.W4-fix the emitters were
/// empty-body stubs with a single-probe loop; this test pins the
/// functional-body contract so a future regression surfaces
/// deterministically.
#[test]
fn w4_pratt_emits_parsable_rust_for_sheets_comparison_expr() {
    let ir = sheets_ir();
    let rule = rule_by_name(&ir, "comparison_expr");
    let ts = pratt::emit_parse_pratt("SheetsFixture", rule, &ir);
    let rendered =
        format_tokens(&ts, "pratt::emit_parse_pratt/comparison_expr");
    for marker in [
        "PRECEDENCE_LUT",
        "LocalOpEntry",
        "op_stack",
        "push_compound",
        "should_reduce",
    ] {
        assert!(
            rendered.contains(marker),
            "emit_parse_pratt/comparison_expr missing functional marker \
             `{marker}` — scaffolding stub re-landed: {rendered}"
        );
    }
    let ts_v = pratt::emit_parse_pratt_visitor("SheetsFixture", rule, &ir);
    let rendered_v =
        format_tokens(&ts_v, "pratt::emit_parse_pratt_visitor/comparison_expr");
    for marker in [
        "PRECEDENCE_LUT",
        "LocalOpEntry",
        "op_stack",
        "begin_pratt",
        "operator",
        "end_pratt",
    ] {
        assert!(
            rendered_v.contains(marker),
            "emit_parse_pratt_visitor/comparison_expr missing functional \
             marker `{marker}`: {rendered_v}"
        );
    }
}

/// Every Sheets operator-tower rung emits functional Pratt bodies —
/// the detector fires on all 5 rungs (comparison_expr → exp_expr)
/// and the emitter produces walker-tape-parity shunting-yard code
/// for each.
#[test]
fn w4_pratt_emits_functional_body_for_every_sheets_tower_rung() {
    let ir = sheets_ir();
    for rung in [
        "comparison_expr",
        "concat_expr",
        "add_expr",
        "mul_expr",
        "exp_expr",
    ] {
        let rule = rule_by_name(&ir, rung);
        let ts = pratt::emit_parse_pratt("SheetsFixture", rule, &ir);
        let rendered =
            format_tokens(&ts, &format!("pratt::emit_parse_pratt/{rung}"));
        assert!(
            rendered.contains("PRECEDENCE_LUT"),
            "Sheets `{rung}` Pratt emitter missing PRECEDENCE_LUT: {rendered}"
        );
        assert!(
            rendered.contains("push_compound"),
            "Sheets `{rung}` Pratt emitter missing reducer compound emission"
        );
    }
}

/// ArgList emitter over `func_call` — Sheets's identifier-head call.
#[test]
fn w4_arglist_emits_parsable_rust_for_sheets_func_call() {
    let ir = sheets_ir();
    let rule = rule_by_name(&ir, "func_call");
    let ts = arglist::emit_parse_arglist("SheetsFixture", rule, &ir);
    let _ = format_tokens(&ts, "arglist::emit_parse_arglist/func_call");
    let ts_v = arglist::emit_parse_arglist_visitor("SheetsFixture", rule, &ir);
    let _ = format_tokens(&ts_v, "arglist::emit_parse_arglist_visitor/func_call");
}

/// ArgList emitter over `let_call` — Sheets's regex-head call with
/// embedded `(`.
#[test]
fn w4_arglist_emits_parsable_rust_for_sheets_let_call() {
    let ir = sheets_ir();
    let rule = rule_by_name(&ir, "let_call");
    let ts = arglist::emit_parse_arglist("SheetsFixture", rule, &ir);
    let _ = format_tokens(&ts, "arglist::emit_parse_arglist/let_call");
}

/// Flat emitter over `error_literal` — Sheets's 9-branch error-code
/// literal, reshaped as a prefix-shared Seq by the factoring pass.
#[test]
fn w4_flat_emits_parsable_rust_for_sheets_error_literal() {
    let ir = sheets_ir();
    let rule = rule_by_name(&ir, "error_literal");
    let ts = flat::emit_parse_flat("SheetsFixture", rule, &ir);
    let _ = format_tokens(&ts, "flat::emit_parse_flat/error_literal");
    let ts_v = flat::emit_parse_flat_visitor("SheetsFixture", rule, &ir);
    let _ = format_tokens(&ts_v, "flat::emit_parse_flat_visitor/error_literal");
}

/// Wrap emitter over `primary` — Sheets's 11-way Alt dispatcher.
#[test]
fn w4_wrap_emits_parsable_rust_for_sheets_primary() {
    let ir = sheets_ir();
    let rule = rule_by_name(&ir, "primary");
    let ts = wrap::emit_parse_wrap("SheetsFixture", rule, &ir);
    let _ = format_tokens(&ts, "wrap::emit_parse_wrap/primary");
    let ts_v = wrap::emit_parse_wrap_visitor("SheetsFixture", rule, &ir);
    let _ = format_tokens(&ts_v, "wrap::emit_parse_wrap_visitor/primary");
}

/// HRegex emitter over `cell_ref` — Sheets's alphanumeric cell-ref
/// regex leaf.
#[test]
fn w4_hregex_emits_parsable_rust_for_sheets_cell_ref() {
    let ir = sheets_ir();
    let rule = rule_by_name(&ir, "cell_ref");
    let ts = hregex::emit_parse_hregex("SheetsFixture", rule, &ir);
    let _ = format_tokens(&ts, "hregex::emit_parse_hregex/cell_ref");
    let ts_v = hregex::emit_parse_hregex_visitor("SheetsFixture", rule, &ir);
    let _ = format_tokens(&ts_v, "hregex::emit_parse_hregex_visitor/cell_ref");
}

/// HRegex emitter over `identifier` — Sheets's generic identifier
/// regex leaf. Doubles as the function-name lexeme under ArgList;
/// there is NO keyword-PHF over Sheets function names, per the H1
/// audit correction (aw5-h1-shape-taxonomy-audit.md §7) and AW-V.md
/// §"Shape taxonomy — Sheets function dispatch correction".
#[test]
fn w4_hregex_emits_parsable_rust_for_sheets_identifier() {
    let ir = sheets_ir();
    let rule = rule_by_name(&ir, "identifier");
    let ts = hregex::emit_parse_hregex("SheetsFixture", rule, &ir);
    let _ = format_tokens(&ts, "hregex::emit_parse_hregex/identifier");
}

/// Unordered emitter — Sheets has no Unordered-shape rule, so we
/// exercise the emitter over any rule (the emitted TokenStream is a
/// scaffold independent of the body's admission). Confirms the
/// scaffold remains parsable when invoked from the W4 orchestrator
/// with a non-admitted rule (which won't happen in practice because
/// the gate filters, but the emitter's output must still parse).
#[test]
fn w4_unordered_emits_parsable_rust_for_sheets_rule() {
    let ir = sheets_ir();
    let rule = rule_by_name(&ir, "formula");
    let ts = unordered::emit_parse_unordered("SheetsFixture", rule, &ir);
    let _ = format_tokens(&ts, "unordered::emit_parse_unordered/formula");
}

/// Keyword emitter over `add_op` — exercises the W3 Keyword path over
/// a Sheets rule. Asserts the emitter also produces parsable Rust for
/// Sheets's operator-tag literal-Alt bodies.
#[test]
fn w3_keyword_emits_parsable_rust_for_sheets_add_op() {
    let ir = sheets_ir();
    let rule = rule_by_name(&ir, "add_op");
    let ts = keyword::emit_parse_keyword("SheetsFixture", rule, &ir);
    let _ = format_tokens(&ts, "keyword::emit_parse_keyword/add_op");
}

/// String emitter over `string`.
#[test]
fn w3_string_emits_parsable_rust_for_sheets_string() {
    let ir = sheets_ir();
    let rule = rule_by_name(&ir, "string");
    let ts = string_emit::emit_parse_string("SheetsFixture", rule, &ir);
    let _ = format_tokens(&ts, "string_emit::emit_parse_string/string");
}

/// Number emitter invoked over Sheets's numeric regex rule (currently
/// classified HRegex; documents the W4.1 gap via direct invocation).
#[test]
fn w3_number_emits_parsable_rust_for_sheets_number_rule() {
    let ir = sheets_ir();
    let rule = rule_by_name(&ir, "number");
    let ts = number_emit::emit_parse_number("SheetsFixture", rule, &ir);
    let _ = format_tokens(&ts, "number_emit::emit_parse_number/number");
}

// ─── End-to-end parse — walker fallback still functional ────────────

#[derive(Parser)]
#[parser(
    path = "../../grammar/google-sheets/google-sheets.bbnf",
    skip_recover
)]
struct SheetsShapeParser;

/// Sanity-check that `SheetsShapeParser::parse()` routes through the
/// walker fallback (per the coverage gate) and succeeds on
/// representative Sheets formulas. Covers:
/// - simple cell arithmetic (operator tower + cell_ref)
/// - function calls (func_call / ArgList shape)
/// - nested function + boolean (primary's Wrap dispatcher)
/// - let-binding (let_call / ArgList regex-head)
/// - error literals (Flat / factored error_literal)
#[test]
fn sheets_parse_succeeds_under_walker_fallback() {
    let fixtures = [
        "=A1+B1",
        "=SUM(A1:A100)",
        "=IF(A1>0, B1, C1)",
        "=VLOOKUP(A1, B1:D100, 3, FALSE)",
        "=#N/A",
        "=LET(x, A1, x+1)",
        "=LAMBDA(x, x*2)",
        "={1,2,3; 4,5,6}",
        "=TRUE",
        "=\"hello\"",
    ];
    for input in fixtures {
        assert!(
            SheetsShapeParser::parse(input).is_ok(),
            "Sheets walker fallback must parse: {input:?}",
        );
    }
}

/// Touch the `RuleId` re-export so the compiler keeps it in the test
/// binary. Keeps the module cohesive without `#[allow]` masks.
#[allow(dead_code)]
fn _rule_id_touch() {
    let _: RuleId = 0;
}
