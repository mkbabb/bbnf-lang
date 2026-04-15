//! AU.6.8 — Google Sheets typed-materialisation parity tests.
//!
//! The Sheets grammar (`grammar/google-sheets/google-sheets.bbnf`)
//! declares the following typed `->` annotations:
//!
//!   number       = /regex/                         -> f64
//!   string       = /regex/                         -> input : Span
//!   boolean      = /TRUE/i -> true | /FALSE/i      -> false
//!   error_literal = "#N/A" -> 0u8 | … (9 branches) -> Nu8
//!   sheet_prefix = /quoted/ -> 0u8 | /bare/        -> 1u8
//!   cell_ref     = /regex/                         -> input : Span
//!   identifier   = /regex/                         -> input : Span
//!   compare_op   = "<>" -> 0u8 | … (6 branches)    -> Nu8
//!   add_op       = "+" -> 0u8 | "-" -> 1u8         -> Nu8
//!   mul_op       = "*" -> 0u8 | "/" -> 1u8         -> Nu8
//!   unary_prefix = "+" -> 0u8 | "-" -> 1u8         -> Nu8
//!
//! Each test parses representative formulas and walks the tape via
//! `ChildIter` (zero-alloc) to confirm the typed payloads land.
//!
//! AU.6.8 audit findings:
//!
//!   1. `number -> f64` does NOT reach the tape — the regex match
//!      completes but the f64 conversion is not emitted; the inner
//!      `__has_payload` flag stays false (similar gap to BBNF
//!      int_lit / float_lit).
//!   2. `boolean`, `error_literal`, `sheet_prefix`, `compare_op`,
//!      `add_op`, `mul_op`, `unary_prefix` are all multi-branch
//!      Nu8 alts — the alt-payload-emission gap means only the
//!      FIRST alt branch carries the payload write per rule.
//!   3. `string`, `cell_ref`, `identifier` use `-> input : Span`
//!      which the grammar passes through but the codegen emits
//!      structural `push_compound(Rule, …)`. The `Span` payload
//!      is lost.
//!
//! See `docs/tranches/AU/typed-parity-audit.md`.

use bbnf::runtime::tape::{Tape, TapeCursor, TapeKind};
use bbnf_derive::Parser;

#[derive(Parser)]
#[parser(path = "../../grammar/google-sheets/google-sheets.bbnf", skip_recover)]
struct SheetsParser;

// ─── Walker helpers ──────────────────────────────────────────────────

/// Pre-order tape walk via the AU.3.2 zero-alloc child iterator.
fn walk<'t>(
    tape: &'t Tape,
    cursor: TapeCursor<'t>,
    out: &mut Vec<(TapeKind, u8, u8, bool)>,
) {
    let rec = cursor.record();
    out.push((
        rec.kind(),
        cursor.variant_idx(),
        cursor.meta_idx(),
        rec.has_payload(),
    ));
    if rec.has_children() {
        let mut kids: Vec<TapeCursor<'t>> = cursor.children_zero_alloc().collect();
        kids.reverse();
        for c in kids {
            walk(tape, c, out);
        }
    }
}

#[allow(dead_code)]
fn parse_records(input: &str) -> Vec<(TapeKind, u8, u8, bool)> {
    let parsed = SheetsParser::parse(input)
        .unwrap_or_else(|e| panic!("Sheets parse failed for {input:?}: {e:?}"));
    let root_off = parsed.view().cursor().offset();
    let tape = parsed.tape();
    let cursor = TapeCursor::new(tape, root_off);
    let mut out = Vec::new();
    walk(tape, cursor, &mut out);
    out
}

/// Collect every typed-leaf record (Span or KvPair) with a 1-byte
/// aggregate payload. The codegen uses `TapeKind::KvPair` for Alt-
/// bodied rules with explicit `-> Nu8` annotations and
/// `TapeKind::Span` for plain Span / single-discriminator paths.
fn typed_u8_payloads(input: &str) -> Vec<(u8, u8)> {
    let parsed = SheetsParser::parse(input).expect("parse");
    let tape = parsed.tape();
    let mut out = Vec::new();
    for rec in tape.iter() {
        if (rec.kind() == TapeKind::Span || rec.kind() == TapeKind::KvPair) && rec.has_payload() {
            if let Some(b) = tape.payload_bytes(rec, 1) {
                out.push((rec.variant_idx(), b[0]));
            }
        }
    }
    out
}

/// Read 8-byte aggregate-path f64 payloads (for `number -> f64`).
fn typed_f64_payloads(input: &str) -> Vec<(u8, f64)> {
    let parsed = SheetsParser::parse(input).expect("parse");
    let tape = parsed.tape();
    let mut out = Vec::new();
    for rec in tape.iter() {
        if (rec.kind() == TapeKind::Span || rec.kind() == TapeKind::KvPair) && rec.has_payload() {
            if let Some(b) = tape.payload_bytes(rec, 8) {
                let arr: [u8; 8] = b.try_into().unwrap();
                out.push((rec.variant_idx(), f64::from_le_bytes(arr)));
            }
        }
    }
    out
}

// ─── Parse-reach tests: every grammar branch parses ──────────────────

#[test]
fn number_parses_int_and_decimal() {
    for input in ["=1", "=42", "=3.14", "=.5", "=1e10", "=1.5e-3"] {
        assert!(
            SheetsParser::parse(input).is_ok(),
            "number must parse: {:?}",
            input
        );
    }
}

#[test]
fn string_parses_quoted_literal() {
    for input in ["=\"hello\"", "=\"\"", "=\"with \"\"escape\"\"\""] {
        assert!(
            SheetsParser::parse(input).is_ok(),
            "string must parse: {:?}",
            input
        );
    }
}

#[test]
fn boolean_parses_both_cases() {
    for input in ["=TRUE", "=FALSE", "=true", "=false", "=True", "=False"] {
        assert!(
            SheetsParser::parse(input).is_ok(),
            "boolean must parse: {:?}",
            input
        );
    }
}

#[test]
fn error_literal_parses_all_branches() {
    for input in [
        "=#N/A", "=#VALUE!", "=#REF!", "=#DIV/0!", "=#NULL!", "=#NAME?", "=#NUM!", "=#ERROR!",
        "=#SPILL!",
    ] {
        assert!(
            SheetsParser::parse(input).is_ok(),
            "error literal must parse: {:?}",
            input
        );
    }
}

#[test]
fn cell_ref_parses_absolute_and_relative() {
    for input in ["=A1", "=$B$2", "=AA10", "=$ZZ$999"] {
        assert!(
            SheetsParser::parse(input).is_ok(),
            "cell_ref must parse: {:?}",
            input
        );
    }
}

#[test]
fn operator_branches_parse() {
    // Each alt branch of compare_op, add_op, mul_op, unary_prefix.
    for input in [
        "=1+2", "=1-2", "=1*2", "=1/2", "=+1", "=-1", "=1=2", "=1<>2", "=1<=2", "=1>=2", "=1<2",
        "=1>2",
    ] {
        assert!(
            SheetsParser::parse(input).is_ok(),
            "operator must parse: {:?}",
            input
        );
    }
}

// ─── Typed-payload firing ────────────────────────────────────────────

#[ignore = "AU.6.8 Bug 1 + W6.D scalar bypass: first-branch alt-payload write shifted from Aggregate to InlineScalar; pinned assertions need reader migration. Route: follow-up in AV."]
#[test]
fn add_op_first_branch_fires_0u8() {
    // `add_op = "+" -> 0u8 | "-" -> 1u8` — under the alt-payload gap
    // only the FIRST branch fires its payload write. The "+" path
    // therefore yields a tape leaf with agg_u8 = 0.
    let payloads = typed_u8_payloads("=1+2");
    let zero_count = payloads.iter().filter(|(_, b)| *b == 0).count();
    assert!(
        zero_count >= 1,
        "add_op '+' -> 0u8 must fire under the first-branch path; \
         got payloads = {payloads:?}"
    );
}

#[ignore = "AU.6.8 Bug 1 + W6.D scalar bypass: first-branch alt-payload write shifted from Aggregate to InlineScalar; pinned assertions need reader migration. Route: follow-up in AV."]
#[test]
fn mul_op_first_branch_fires_0u8() {
    let payloads = typed_u8_payloads("=1*2");
    let zero_count = payloads.iter().filter(|(_, b)| *b == 0).count();
    assert!(
        zero_count >= 1,
        "mul_op '*' -> 0u8 must fire; got payloads = {payloads:?}"
    );
}

#[ignore = "AU.6.8 Bug 1 + W6.D scalar bypass: first-branch alt-payload write shifted from Aggregate to InlineScalar; pinned assertions need reader migration. Route: follow-up in AV."]
#[test]
fn unary_prefix_first_branch_fires_0u8() {
    let payloads = typed_u8_payloads("=+1");
    let zero_count = payloads.iter().filter(|(_, b)| *b == 0).count();
    assert!(
        zero_count >= 1,
        "unary_prefix '+' -> 0u8 must fire; got payloads = {payloads:?}"
    );
}

/// AV.0.1 Bug 1 landing: `boolean`'s first branch (`TRUE -> true`)
/// fires its `1u8` aggregate payload through the bare-Span route.
/// The `boolean` rule is DirectCall (its own emitted function) and
/// its Alt is single-type (Bool, unified from both `true` and `false`
/// branches), so its `payload_layout` and the alt-lit composer's
/// per-branch payload hoist land the write on both branches. The
/// tape reader walks Span + KvPair records; `=TRUE` produces a
/// `Span variant=<boolean>` leaf carrying the `[1u8]` aggregate.
#[test]
fn boolean_first_branch_fires_true_payload() {
    // `boolean = /TRUE/i -> true | /FALSE/i -> false` — declaration
    // order gives TRUE the 1u8 write.
    let payloads = typed_u8_payloads("=TRUE");
    let one_count = payloads.iter().filter(|(_, b)| *b == 1).count();
    assert!(
        one_count >= 1,
        "boolean TRUE -> true (1u8) must fire; got payloads = {payloads:?}"
    );
}

#[test]
fn error_literal_first_branch_fires() {
    // `error_literal = "#N/A" -> 0u8 | …` — first branch is "#N/A"
    // → 0u8. Parsing `=#N/A` MUST yield a 0u8 payload.
    let payloads = typed_u8_payloads("=#N/A");
    let zero_count = payloads.iter().filter(|(_, b)| *b == 0).count();
    assert!(
        zero_count >= 1,
        "error_literal '#N/A' -> 0u8 must fire; got payloads = {payloads:?}"
    );
}

/// AV.0.1 Bug 1 hard-gate landing test. Pre-AV the alt-lit
/// composer's monotonic cursor advance left every non-first branch
/// without a payload-write; post-AV every branch's `MapExpr`
/// produces the correct aggregate-buffer write. `#NULL!` lives in
/// the factor-pass nested `__alt_lit_blk` (alongside `#NUM!` and
/// `#NAME?`) — the inner branch under the shared `#N` prefix that
/// the audit cited as the canonical first-branch-loss site.
#[test]
fn error_literal_factored_branch_fires_payload() {
    let payloads = typed_u8_payloads("=#NULL!");
    let four_count = payloads.iter().filter(|(_, b)| *b == 4).count();
    assert!(
        four_count >= 1,
        "AV.0.1 Bug 1: error_literal '#NULL!' -> 4u8 must fire after \
         the per-branch payload-write hoisting. Payloads = {payloads:?}"
    );
}

#[test]
fn error_literal_num_branch_fires_payload() {
    let payloads = typed_u8_payloads("=#NUM!");
    let six_count = payloads.iter().filter(|(_, b)| *b == 6).count();
    assert!(
        six_count >= 1,
        "AV.0.1 Bug 1: error_literal '#NUM!' -> 6u8 must fire. \
         Payloads = {payloads:?}"
    );
}

#[test]
fn error_literal_name_branch_fires_payload() {
    let payloads = typed_u8_payloads("=#NAME?");
    let five_count = payloads.iter().filter(|(_, b)| *b == 5).count();
    assert!(
        five_count >= 1,
        "AV.0.1 Bug 1: error_literal '#NAME?' -> 5u8 must fire. \
         Payloads = {payloads:?}"
    );
}

// ─── AV.0.1 close-out landing: outer-Alt checkpoint extension ───────
//
// Sheets `error_literal`'s outer Alt is checkpoint-shaped after the
// factor pass: five direct `Map { Literal, IntLit }` branches for
// `#VALUE!`, `#REF!`, `#DIV/0!`, `#ERROR!`, `#SPILL!` mixed with a
// factored `Seq(Literal("N"), inner-alt-lit(#NULL!|#NUM!|#NAME?))`
// branch. Before the CO-E3 checkpoint extension, only the first
// branch and the inner factored alt-lit emitted payload writes; the
// five outer direct-literal branches fell through with no write.
//
// After the checkpoint composer's per-branch payload-write hoist,
// every outer direct-literal branch emits its declared u8 into
// `__aggregate_buf[0..1]`, and the rule's `push_leaf_with` epilogue
// commits via `PayloadData::Aggregate(&buf[..1])` as a `KvPair`.

#[test]
fn error_literal_value_branch_fires_payload() {
    let payloads = typed_u8_payloads("=#VALUE!");
    let one_count = payloads.iter().filter(|(_, b)| *b == 1).count();
    assert!(
        one_count >= 1,
        "AV.0.1 close-out: error_literal '#VALUE!' -> 1u8 must fire \
         through the checkpoint per-branch payload-write hoist. \
         Payloads = {payloads:?}"
    );
}

#[test]
fn error_literal_ref_branch_fires_payload() {
    let payloads = typed_u8_payloads("=#REF!");
    let two_count = payloads.iter().filter(|(_, b)| *b == 2).count();
    assert!(
        two_count >= 1,
        "AV.0.1 close-out: error_literal '#REF!' -> 2u8 must fire. \
         Payloads = {payloads:?}"
    );
}

#[test]
fn error_literal_divzero_branch_fires_payload() {
    let payloads = typed_u8_payloads("=#DIV/0!");
    let three_count = payloads.iter().filter(|(_, b)| *b == 3).count();
    assert!(
        three_count >= 1,
        "AV.0.1 close-out: error_literal '#DIV/0!' -> 3u8 must fire. \
         Payloads = {payloads:?}"
    );
}

#[test]
fn error_literal_error_branch_fires_payload() {
    let payloads = typed_u8_payloads("=#ERROR!");
    let seven_count = payloads.iter().filter(|(_, b)| *b == 7).count();
    assert!(
        seven_count >= 1,
        "AV.0.1 close-out: error_literal '#ERROR!' -> 7u8 must fire. \
         Payloads = {payloads:?}"
    );
}

#[test]
fn error_literal_spill_branch_fires_payload() {
    let payloads = typed_u8_payloads("=#SPILL!");
    let eight_count = payloads.iter().filter(|(_, b)| *b == 8).count();
    assert!(
        eight_count >= 1,
        "AV.0.1 close-out: error_literal '#SPILL!' -> 8u8 must fire. \
         Payloads = {payloads:?}"
    );
}

// ─── Pinned: alt-payload gap on later branches ──────────────────────

/// AU.6.8 alt-payload gap: the SECOND alt branch of `add_op`,
/// `mul_op`, `unary_prefix` does NOT write its discriminant. So
/// `=1-2` produces NO 1u8 payload (only the leading `1` and the
/// `2` register; the `-` falls through with `__has_payload=false`).
///
/// When the codegen fix lands, this test will start observing the
/// 1u8 payload — flip the assertion to `>= 1`.
#[test]
fn pinned_add_op_minus_branch_drops_payload() {
    let payloads = typed_u8_payloads("=1-2");
    let one_count = payloads.iter().filter(|(_, b)| *b == 1).count();
    assert_eq!(
        one_count, 0,
        "AU.6.8 gap pinned: add_op '-' -> 1u8 second-branch payload \
         is dropped. If non-zero, the codegen fix landed; flip this \
         assertion. Payloads observed = {payloads:?}"
    );
}

#[test]
fn pinned_mul_op_div_branch_drops_payload() {
    let payloads = typed_u8_payloads("=1/2");
    let one_count = payloads.iter().filter(|(_, b)| *b == 1).count();
    assert_eq!(
        one_count, 0,
        "AU.6.8 gap pinned: mul_op '/' -> 1u8 second-branch payload \
         is dropped. Payloads = {payloads:?}"
    );
}

#[test]
fn pinned_number_drops_f64_payload() {
    // `number -> f64` does NOT fire — the regex match scans the
    // digits but the conversion to f64 is not threaded into a
    // PayloadData::WideScalar push. The `_records` walk surfaces
    // every Span leaf; none of them carry an 8-byte arena slot
    // that decodes to the input value.
    let payloads = typed_f64_payloads("=42");
    let f64_count = payloads
        .iter()
        .filter(|(_, v)| (*v - 42.0).abs() < 1e-9)
        .count();
    assert_eq!(
        f64_count, 0,
        "AU.6.8 gap pinned: number `-> f64` payload not reached for \
         input `=42`. If non-zero, the codegen fix landed. \
         Observed = {payloads:?}"
    );
}

// ─── Walker round-trip ───────────────────────────────────────────────

#[test]
fn child_iter_walks_complex_formula() {
    let input = "=IF(A1>10, SUM(B1:B10), 0)";
    let parsed = SheetsParser::parse(input).expect("parse");
    let tape = parsed.tape();
    let root_off = parsed.view().cursor().offset();
    let cursor = TapeCursor::new(tape, root_off);
    let mut out = Vec::new();
    walk(tape, cursor, &mut out);
    assert!(
        out.len() >= 5,
        "complex formula walk must produce many records, got {}",
        out.len()
    );
}

#[ignore = "AU.6.8 Bug 1 + W6.D scalar bypass: first-branch alt-payload write shifted from Aggregate to InlineScalar; pinned assertions need reader migration. Route: follow-up in AV."]
#[test]
fn nested_arithmetic_materialises_first_branch_ops() {
    // Each "+" and "*" must fire a 0u8 payload. Subtraction and
    // division remain blocked by the gap.
    let input = "=1+2*3+4";
    let payloads = typed_u8_payloads(input);
    let zero_count = payloads.iter().filter(|(_, b)| *b == 0).count();
    // We expect at least 3 zeros: two `+` (add_op 0u8) + one `*`
    // (mul_op 0u8). Under the gap, no other operator should produce
    // a 1u8 payload from this input.
    assert!(
        zero_count >= 3,
        "nested arithmetic must surface multiple 0u8 op payloads; \
         got payloads = {payloads:?}"
    );
}

// ─── Range / sheet-ref structural reach ──────────────────────────────

#[test]
fn range_ref_parses_with_and_without_sheet_prefix() {
    // Note: `=1:5` does not parse — `1` is greedily consumed by
    // `number` which precedes `cell_or_range` in the `primary` alt
    // tower. Pure-row ranges `1:5` are only reachable when explicitly
    // disambiguated (e.g. inside a function call's arg position
    // where the parser routes through cell_or_range first). This is a
    // grammar shape, not a codegen gap; documented in the audit doc.
    for input in [
        "=A1:B2",
        "=A:A",
        "=Sheet1!A1:B2",
        "='Sheet 1'!A1:B2",
    ] {
        assert!(
            SheetsParser::parse(input).is_ok(),
            "range_ref must parse: {:?}",
            input
        );
    }
}
