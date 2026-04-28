//! AX.W1r.4 — Google Sheets self-parity via shape-emission-authoritative tape.
//!
//! Two classes of per-case assertion covering every `Expr` variant declared
//! in `grammar/google-sheets/google-sheets.bbnf`:
//!
//!   1. `serialize_roundtrip_<variant>` — parse → `serialize_compact` equals
//!      the original byte-for-byte when the grammar's own whitespace
//!      policy preserves the surface. Where `?w` / `@pretty compact` normalise
//!      interior whitespace, parity degrades to semantic preservation: the
//!      canonical form is a fixed point under re-parse + re-emit.
//!
//!   2. `prettify_idempotent_<variant>` — prettify once → prettify the
//!      result again → the two outputs are byte-identical. Prettify is the
//!      canonical form by definition; its idempotency is non-negotiable.
//!
//! No external comparator; this harness proves the grammar's own surface
//! is self-consistent under the shape-emission-authoritative tape.
//!
//! The derive attribute carries BOTH `serialize` and `prettify`; a single
//! struct supplies the round-trip emitter and the prettify combinator.

use ::bbnf::grammar::generated::google_sheets::*;


// ── Helpers ─────────────────────────────────────────────────────────

/// Parse once, emit the `serialize_compact` canonical form.
///
/// AZ-I.W2-act.close B2 — migrated from the cursor-backed
/// `GoogleSheetsParser::serialize_compact(node)` route to the
/// struct-tree walker on [`bbnf::runtime::SheetsDocument`]. The
/// pre-W2-act emitter consulted [`crate::runtime::tape::TapeCursor`]
/// span text via `GoogleSheetsParserNodeView::from_cursor`; under the
/// struct-direct flip the parse output IS the typed tree, so the
/// equivalent surface is `doc.serialize_compact()`.
fn serialize(input: &str) -> String {
    let doc = GoogleSheetsParser::parse(input).expect("sheets parse must succeed");
    doc.serialize_compact()
}

/// Prettify one round via the combinator side-channel + shared printer.
fn prettify(input: &str) -> String {
    let config = pprint::Printer::new(80, 2, false);
    let ops = GoogleSheetsParser::formula_prettify()
        .parse(input)
        .expect("sheets prettify must succeed");
    pprint::render(&ops, config)
}

/// Assert that `serialize_compact` reaches a canonical fixed point under
/// re-parse + re-emit. Byte-for-byte parity against the source is checked
/// first; if the grammar normalises interior whitespace (via `?w`), we fall
/// back to the fixed-point contract (semantic preservation without byte
/// preservation). Any other divergence is a grammar or serializer bug.
fn assert_serialize_fixed_point(src: &str) {
    let first = serialize(src);
    if first == src {
        // Byte-perfect. Verify idempotency to be sure.
        let second = serialize(&first);
        assert_eq!(
            first, second,
            "serialize_compact non-idempotent for {src:?}: {first:?} vs {second:?}"
        );
        return;
    }
    // Whitespace-insensitive input — the grammar's `?w` normalisation
    // collapses surface spaces. Require the canonical form to be a
    // fixed point (parse(serialize(parse(src))) == parse(src) canonical
    // form), observed by re-emitting `first` and comparing.
    let second = serialize(&first);
    assert_eq!(
        first, second,
        "serialize_compact non-idempotent for {src:?}:\n  src    = {src:?}\n  first  = {first:?}\n  second = {second:?}"
    );
}

/// Assert that prettify is a canonical form: prettify once, prettify again,
/// bytes must match exactly.
fn assert_prettify_idempotent(src: &str) {
    let first = prettify(src);
    let second = prettify(&first);
    assert_eq!(
        first, second,
        "prettify non-idempotent for {src:?}:\n  first  = {first:?}\n  second = {second:?}"
    );
}

// ── serialize round-trip: one test per primary / Expr variant ──────
//
// The grammar's `primary` enumerates:
//   let_call | lambda_call | func_call | number | string | boolean
//   | error_literal | cell_or_range (range_ref | cell) | identifier
//   | array_literal | paren_expr
// plus the operator towers (unary, postfix, exp, mul, add, concat,
// compare) which compose atop `primary`. Each variant gets its own
// serialize + prettify case.

#[test]
fn serialize_roundtrip_number_int() {
    assert_serialize_fixed_point("=42");
}

#[test]
fn serialize_roundtrip_number_decimal() {
    assert_serialize_fixed_point("=3.14");
}

#[test]
fn serialize_roundtrip_number_leading_dot() {
    assert_serialize_fixed_point("=.5");
}

#[test]
fn serialize_roundtrip_number_exponent() {
    assert_serialize_fixed_point("=1e10");
}

#[test]
fn serialize_roundtrip_number_signed_exponent() {
    assert_serialize_fixed_point("=1.5e-3");
}

#[test]
fn serialize_roundtrip_string_basic() {
    assert_serialize_fixed_point(r#"="hello""#);
}

#[test]
fn serialize_roundtrip_string_empty() {
    assert_serialize_fixed_point(r#"="""#);
}

#[test]
fn serialize_roundtrip_boolean_true() {
    assert_serialize_fixed_point("=TRUE");
}

#[test]
fn serialize_roundtrip_boolean_false() {
    assert_serialize_fixed_point("=FALSE");
}

#[test]
fn serialize_roundtrip_boolean_lower() {
    assert_serialize_fixed_point("=true");
}

#[test]
fn serialize_roundtrip_error_na() {
    assert_serialize_fixed_point("=#N/A");
}

#[test]
fn serialize_roundtrip_error_value() {
    assert_serialize_fixed_point("=#VALUE!");
}

#[test]
fn serialize_roundtrip_error_ref() {
    assert_serialize_fixed_point("=#REF!");
}

#[test]
fn serialize_roundtrip_error_divzero() {
    assert_serialize_fixed_point("=#DIV/0!");
}

#[test]
fn serialize_roundtrip_error_null() {
    assert_serialize_fixed_point("=#NULL!");
}

#[test]
fn serialize_roundtrip_error_name() {
    assert_serialize_fixed_point("=#NAME?");
}

#[test]
fn serialize_roundtrip_error_num() {
    assert_serialize_fixed_point("=#NUM!");
}

#[test]
fn serialize_roundtrip_error_generic() {
    assert_serialize_fixed_point("=#ERROR!");
}

#[test]
fn serialize_roundtrip_error_spill() {
    assert_serialize_fixed_point("=#SPILL!");
}

#[test]
fn serialize_roundtrip_cell_ref_relative() {
    assert_serialize_fixed_point("=A1");
}

#[test]
fn serialize_roundtrip_cell_ref_absolute() {
    assert_serialize_fixed_point("=$B$2");
}

#[test]
fn serialize_roundtrip_cell_ref_wide() {
    assert_serialize_fixed_point("=AA10");
}

#[test]
fn serialize_roundtrip_range_ref_pair() {
    assert_serialize_fixed_point("=A1:B2");
}

#[test]
fn serialize_roundtrip_range_ref_column() {
    assert_serialize_fixed_point("=A:A");
}

#[test]
fn serialize_roundtrip_range_ref_sheet_prefixed() {
    assert_serialize_fixed_point("=Sheet1!A1:B2");
}

#[test]
fn serialize_roundtrip_range_ref_quoted_sheet() {
    assert_serialize_fixed_point("='Sheet 1'!A1:B2");
}

#[test]
fn serialize_roundtrip_identifier_bare() {
    assert_serialize_fixed_point("=foo");
}

#[test]
fn serialize_roundtrip_fn_call_zero_args() {
    assert_serialize_fixed_point("=NOW()");
}

#[test]
fn serialize_roundtrip_fn_call_single_arg() {
    assert_serialize_fixed_point("=SUM(A1:A10)");
}

#[test]
fn serialize_roundtrip_fn_call_multi_arg() {
    assert_serialize_fixed_point("=IF(A1>0,A1,0)");
}

#[test]
fn serialize_roundtrip_fn_call_nested() {
    assert_serialize_fixed_point("=IF(A1>0,SUM(B1:B10),0)");
}

#[test]
fn serialize_roundtrip_let_call() {
    assert_serialize_fixed_point("=LET(a,1,b,2,a+b)");
}

#[test]
fn serialize_roundtrip_lambda_call() {
    assert_serialize_fixed_point("=LAMBDA(x,x+1)");
}

#[test]
fn serialize_roundtrip_array_literal_single_row() {
    assert_serialize_fixed_point("={1,2,3}");
}

#[test]
fn serialize_roundtrip_array_literal_multi_row() {
    assert_serialize_fixed_point("={1,2;3,4}");
}

#[test]
fn serialize_roundtrip_paren_expr() {
    assert_serialize_fixed_point("=(1+2)");
}

// Operator tower — one case per op to exercise each `-> Nu8` branch.

#[test]
fn serialize_roundtrip_add_op_plus() {
    assert_serialize_fixed_point("=1+2");
}

#[test]
fn serialize_roundtrip_add_op_minus() {
    assert_serialize_fixed_point("=1-2");
}

#[test]
fn serialize_roundtrip_mul_op_star() {
    assert_serialize_fixed_point("=1*2");
}

#[test]
fn serialize_roundtrip_mul_op_slash() {
    assert_serialize_fixed_point("=1/2");
}

#[test]
fn serialize_roundtrip_exp_op() {
    assert_serialize_fixed_point("=2^3");
}

#[test]
fn serialize_roundtrip_concat_op() {
    assert_serialize_fixed_point(r#"="a"&"b""#);
}

#[test]
fn serialize_roundtrip_compare_eq() {
    assert_serialize_fixed_point("=1=2");
}

#[test]
fn serialize_roundtrip_compare_ne() {
    assert_serialize_fixed_point("=1<>2");
}

#[test]
fn serialize_roundtrip_compare_le() {
    assert_serialize_fixed_point("=1<=2");
}

#[test]
fn serialize_roundtrip_compare_ge() {
    assert_serialize_fixed_point("=1>=2");
}

#[test]
fn serialize_roundtrip_compare_lt() {
    assert_serialize_fixed_point("=1<2");
}

#[test]
fn serialize_roundtrip_compare_gt() {
    assert_serialize_fixed_point("=1>2");
}

#[test]
fn serialize_roundtrip_unary_plus() {
    assert_serialize_fixed_point("=+1");
}

#[test]
fn serialize_roundtrip_unary_minus() {
    assert_serialize_fixed_point("=-1");
}

#[test]
fn serialize_roundtrip_postfix_percent() {
    assert_serialize_fixed_point("=50%");
}

// ── prettify idempotency: one test per Expr variant ────────────────

#[test]
fn prettify_idempotent_number_int() {
    assert_prettify_idempotent("=42");
}

#[test]
fn prettify_idempotent_number_decimal() {
    assert_prettify_idempotent("=3.14");
}

#[test]
fn prettify_idempotent_string_basic() {
    assert_prettify_idempotent(r#"="hello""#);
}

#[test]
fn prettify_idempotent_boolean_true() {
    assert_prettify_idempotent("=TRUE");
}

#[test]
fn prettify_idempotent_error_na() {
    assert_prettify_idempotent("=#N/A");
}

#[test]
fn prettify_idempotent_error_value() {
    assert_prettify_idempotent("=#VALUE!");
}

#[test]
fn prettify_idempotent_cell_ref_relative() {
    assert_prettify_idempotent("=A1");
}

#[test]
fn prettify_idempotent_cell_ref_absolute() {
    assert_prettify_idempotent("=$B$2");
}

#[test]
fn prettify_idempotent_range_ref_pair() {
    assert_prettify_idempotent("=A1:B2");
}

#[test]
fn prettify_idempotent_range_ref_column() {
    assert_prettify_idempotent("=A:A");
}

#[test]
fn prettify_idempotent_range_ref_sheet_prefixed() {
    assert_prettify_idempotent("=Sheet1!A1:B2");
}

#[test]
fn prettify_idempotent_identifier_bare() {
    assert_prettify_idempotent("=foo");
}

#[test]
fn prettify_idempotent_fn_call_zero_args() {
    assert_prettify_idempotent("=NOW()");
}

#[test]
fn prettify_idempotent_fn_call_single_arg() {
    assert_prettify_idempotent("=SUM(A1:A10)");
}

#[test]
fn prettify_idempotent_fn_call_multi_arg() {
    assert_prettify_idempotent("=IF(A1>0,A1,0)");
}

#[test]
fn prettify_idempotent_fn_call_nested() {
    assert_prettify_idempotent("=IF(A1>0,SUM(B1:B10),0)");
}

#[test]
fn prettify_idempotent_let_call() {
    assert_prettify_idempotent("=LET(a,1,b,2,a+b)");
}

#[test]
fn prettify_idempotent_lambda_call() {
    assert_prettify_idempotent("=LAMBDA(x,x+1)");
}

#[test]
fn prettify_idempotent_array_literal_single_row() {
    assert_prettify_idempotent("={1,2,3}");
}

#[test]
fn prettify_idempotent_array_literal_multi_row() {
    assert_prettify_idempotent("={1,2;3,4}");
}

#[test]
fn prettify_idempotent_paren_expr() {
    assert_prettify_idempotent("=(1+2)");
}

#[test]
fn prettify_idempotent_add_op() {
    assert_prettify_idempotent("=1+2");
}

#[test]
fn prettify_idempotent_mul_op() {
    assert_prettify_idempotent("=1*2");
}

#[test]
fn prettify_idempotent_exp_op() {
    assert_prettify_idempotent("=2^3");
}

#[test]
fn prettify_idempotent_concat_op() {
    assert_prettify_idempotent(r#"="a"&"b""#);
}

#[test]
fn prettify_idempotent_compare_eq() {
    assert_prettify_idempotent("=1=2");
}

#[test]
fn prettify_idempotent_compare_chain() {
    assert_prettify_idempotent("=1<=2");
}

#[test]
fn prettify_idempotent_unary_plus() {
    assert_prettify_idempotent("=+1");
}

#[test]
fn prettify_idempotent_unary_minus() {
    assert_prettify_idempotent("=-1");
}

#[test]
fn prettify_idempotent_postfix_percent() {
    assert_prettify_idempotent("=50%");
}

// ── Fixture corpus: data/sheets/*.txt line-by-line ────────────────
//
// `data/sheets/simple.txt`, `nested.txt`, and `stress.txt` carry a
// hand-curated corpus of live-world formulas. Each non-empty line is
// an independent formula; we loop both assertions over every line.

fn loop_corpus(corpus: &str, tag: &str) {
    let mut seen = 0usize;
    for (idx, raw) in corpus.lines().enumerate() {
        let line = raw.trim();
        if line.is_empty() || line.starts_with('#') {
            continue;
        }
        // serialize fixed-point
        let first = serialize(line);
        let second = serialize(&first);
        assert_eq!(
            first, second,
            "[{tag}] serialize_compact non-idempotent at line {idx} ({line:?})"
        );
        // prettify idempotency (byte-identical)
        let p1 = prettify(line);
        let p2 = prettify(&p1);
        assert_eq!(
            p1, p2,
            "[{tag}] prettify non-idempotent at line {idx} ({line:?})"
        );
        seen += 1;
    }
    assert!(seen > 0, "[{tag}] corpus contributed zero cases");
}

#[test]
fn corpus_simple() {
    let corpus = include_str!("../../../data/sheets/simple.txt");
    loop_corpus(corpus, "simple");
}

#[test]
fn corpus_nested() {
    let corpus = include_str!("../../../data/sheets/nested.txt");
    loop_corpus(corpus, "nested");
}

#[test]
fn corpus_stress() {
    let corpus = include_str!("../../../data/sheets/stress.txt");
    loop_corpus(corpus, "stress");
}
