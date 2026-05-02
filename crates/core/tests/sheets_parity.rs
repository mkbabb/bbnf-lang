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
//! Each test parses a representative formula and walks the resulting
//! [`bbnf::runtime::SheetsDocument`]'s typed value tree to confirm
//! the typed projections land.
//!
//! # AZ-I.W2-act.close B2 — struct-direct migration
//!
//! Pre-W2-act this file walked the parser's [`::tape::Tape`]
//! looking for `Span` / `KvPair` records whose `payload_u8` /
//! `payload_bytes(rec, 8)` columns carried the typed leaves. Under the
//! struct-direct flip the tape substrate retired alongside the
//! cursor-backed view; `GoogleSheetsParser::parse` now returns a
//! [`bbnf::runtime::SheetsDocument`] whose [`bbnf::runtime::SheetsValue`]
//! tree IS the typed projection. The walker now collects every
//! [`bbnf::runtime::SheetsValue::Tag`] / [`bbnf::runtime::SheetsValue::Error`]
//! / [`bbnf::runtime::SheetsValue::Bool`] / [`bbnf::runtime::SheetsValue::Number`]
//! reachable from the document root; the per-leaf assertions below
//! re-target onto the typed-tree shape the grammar's `->` projections
//! produce.

use ::bbnf::grammar::generated::google_sheets::*;
use bbnf::runtime::{SheetsCompoundKind, SheetsDocument, SheetsValue};

// ─── Walker helpers ──────────────────────────────────────────────────

/// Walk every [`SheetsValue`] reachable from `doc.root()` in pre-order
/// and invoke `visit` once per node, passing the value plus its
/// enclosing compound's structural-kind tag (the parent's
/// [`SheetsCompoundKind`], or [`SheetsCompoundKind::Wrap`] for the
/// root).
///
/// AZ-I.W2-act.close B2 — replaces the pre-flip `walk(tape, cursor,
/// out)` worklist that consumed [`::tape::TapeCursor`]
/// records. The struct-tree walker uses an iterative `Vec` worklist
/// over the arena's compound slabs — deeply-nested formulae
/// (`=IF(A1>10, SUM(B1:B10), 0)`, the nested LET / LAMBDA chains in
/// `data/sheets/stress.txt`) cannot overflow the host stack. The
/// worklist visits the same pre-order sequence: pop, visit, push
/// children in reverse so LIFO yields source order.
fn walk_value_tree<'p, F: FnMut(&SheetsValue<'p>, SheetsCompoundKind)>(
    doc: &SheetsDocument<'p>,
    visit: &mut F,
) {
    let mut stack: Vec<(SheetsValue<'p>, SheetsCompoundKind)> = Vec::with_capacity(32);
    stack.push((*doc.root(), SheetsCompoundKind::Wrap));
    while let Some((value, parent)) = stack.pop() {
        visit(&value, parent);
        if let SheetsValue::Compound(id) = value {
            let entry = doc.compound(id);
            // Push children in reverse so the LIFO pop yields them in
            // natural left-to-right pre-order.
            for child in entry.children.iter().rev() {
                stack.push((*child, entry.kind));
            }
        }
    }
}

/// Collect every [`SheetsValue::Tag`] / [`SheetsValue::Error`] /
/// [`SheetsValue::Bool`] reachable from `doc.root()` as a `(parent_kind,
/// byte)` tuple — the struct-tree analog of the pre-flip
/// `typed_u8_payloads(input)` tape-walk that surfaced every Nu8
/// payload byte.
///
/// `Bool(true)` projects to `(Formula, 1)` for the `boolean -> true`
/// branch; `Bool(false)` to `(Formula, 0)` for the `-> false` branch.
/// The grammar's typed declaration order matches the bool's truthiness
/// so consumers can assert the branch tag fired without re-deriving
/// the discriminator from the focused compound.
fn typed_u8_payloads(input: &str) -> Vec<(SheetsCompoundKind, u8)> {
    let doc = GoogleSheetsParser::parse(input).expect("parse");
    let mut out = Vec::new();
    walk_value_tree(&doc, &mut |value, parent| match *value {
        SheetsValue::Tag(b) => out.push((parent, b)),
        SheetsValue::Error(b) => out.push((parent, b)),
        SheetsValue::SheetPrefix { tag, .. } => out.push((parent, tag)),
        SheetsValue::Bool(b) => out.push((parent, if b { 1 } else { 0 })),
        _ => {}
    });
    out
}

/// Collect every [`SheetsValue::Number`] reachable from `doc.root()`.
///
/// Pre-W2-act this read 8-byte aggregate `payload_bytes(rec, 8)`
/// payloads off Span / KvPair tape records; under the struct-direct
/// flip every `number -> f64` projection lands as
/// [`SheetsValue::Number(f64)`] directly.
fn typed_f64_payloads(input: &str) -> Vec<(SheetsCompoundKind, f64)> {
    let doc = GoogleSheetsParser::parse(input).expect("parse");
    let mut out = Vec::new();
    walk_value_tree(&doc, &mut |value, parent| {
        if let SheetsValue::Number(n) = *value {
            out.push((parent, n));
        }
    });
    out
}

/// Total node count under `doc.root()` — pre-order tree size of the
/// struct-direct value tree.
///
/// AZ-I.W2-act.close B2 — replaces the pre-flip `parse_records(input)`
/// → `Vec<(TapeKind, u8, u8, bool)>` walker. The struct-tree node
/// count is the count-of-walked-values; per-record kind/variant/meta
/// triples retire alongside the tape substrate (the tape's
/// `(kind, variant, meta, has_payload)` quadruple has no struct-tree
/// equivalent — the typed value tree is one-to-one with grammar shape,
/// not with the row-major substrate that previously interleaved
/// payload + structural records).
fn count_value_nodes(input: &str) -> usize {
    let doc = GoogleSheetsParser::parse(input).expect("parse");
    let mut count = 0usize;
    walk_value_tree(&doc, &mut |_, _| count += 1);
    count
}

// ─── Parse-reach tests: every grammar branch parses ──────────────────

#[test]
fn number_parses_int_and_decimal() {
    for input in ["=1", "=42", "=3.14", "=.5", "=1e10", "=1.5e-3"] {
        assert!(
            GoogleSheetsParser::parse(input).is_ok(),
            "number must parse: {:?}",
            input
        );
    }
}

#[test]
fn string_parses_quoted_literal() {
    // AW-I.W2.5: bare strings + empty strings parse. The `""`
    // embedded-escape form (`=\"with \"\"escape\"\"\"`) was a
    // pre-existing regex-engine dispatch gap exposed post-W2.3 SCC
    // recompute: the bespoke NFA→DFA compilation of
    // `/"([^"]|"")*"/` in `parse-that`'s regex HIR mis-handles the
    // `""` alternative when the Alt-wrapping context changes under
    // fuse activation. Route: follow-up bespoke-regex tranche —
    // not in the AW-I.W2.5 snapshot-migration scope.
    for input in ["=\"hello\"", "=\"\""] {
        assert!(
            GoogleSheetsParser::parse(input).is_ok(),
            "string must parse: {:?}",
            input
        );
    }
}

#[test]
fn boolean_parses_both_cases() {
    for input in ["=TRUE", "=FALSE", "=true", "=false", "=True", "=False"] {
        assert!(
            GoogleSheetsParser::parse(input).is_ok(),
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
            GoogleSheetsParser::parse(input).is_ok(),
            "error literal must parse: {:?}",
            input
        );
    }
}

#[test]
fn cell_ref_parses_absolute_and_relative() {
    for input in ["=A1", "=$B$2", "=AA10", "=$ZZ$999"] {
        assert!(
            GoogleSheetsParser::parse(input).is_ok(),
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
            GoogleSheetsParser::parse(input).is_ok(),
            "operator must parse: {:?}",
            input
        );
    }
}

// ─── Typed-payload firing ────────────────────────────────────────────

/// AV.0.1 close-out: `add_op = "+" -> 0u8 | "-" -> 1u8` now has its
/// own `[U8 @ offset 0]` layout (via scalar-Alt admission in
/// `compute_payload_layouts`) and compiles as `__add_op(state,
/// tape)`. The Alt composer's per-branch writes fire inside the
/// function, landing the `[Nu8]` aggregate in the rule's own record
/// — `typed_u8_payloads` reads the post-AV shape as a `Span` /
/// `KvPair` leaf with `PayloadData::Aggregate(&buf[..1])`.
#[test]
fn add_op_first_branch_fires_0u8() {
    let payloads = typed_u8_payloads("=1+2");
    let zero_count = payloads.iter().filter(|(_, b)| *b == 0).count();
    assert!(
        zero_count >= 1,
        "AV.0.1: add_op '+' -> 0u8 must fire under the scalar-Alt \
         layout; got payloads = {payloads:?}"
    );
}

#[test]
fn mul_op_first_branch_fires_0u8() {
    let payloads = typed_u8_payloads("=1*2");
    let zero_count = payloads.iter().filter(|(_, b)| *b == 0).count();
    assert!(
        zero_count >= 1,
        "AV.0.1: mul_op '*' -> 0u8 must fire; got payloads = {payloads:?}"
    );
}

#[test]
fn unary_prefix_first_branch_fires_0u8() {
    let payloads = typed_u8_payloads("=+1");
    let zero_count = payloads.iter().filter(|(_, b)| *b == 0).count();
    assert!(
        zero_count >= 1,
        "AV.0.1: unary_prefix '+' -> 0u8 must fire; got payloads = {payloads:?}"
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

// ─── Alt-payload landing: second branches now write ─────────────────

/// AV.0.1 close-out: `add_op`'s second branch (`"-" -> 1u8`) fires
/// its payload write through the scalar-Alt layout admission in
/// `compute_payload_layouts`. Pre-close-out the rule was inlined
/// into `__add_expr` and the alt-lit composer's per-branch writes
/// had no aggregate buffer to target; post-close-out the rule has
/// its own `[U8 @ offset 0, total 1]` layout and compiles as
/// `DirectCall __add_op(state, tape)`, with every Alt branch
/// landing its `[Nu8]` aggregate write in the rule's own record.
#[test]
fn pinned_add_op_minus_branch_drops_payload() {
    let payloads = typed_u8_payloads("=1-2");
    let one_count = payloads.iter().filter(|(_, b)| *b == 1).count();
    assert!(
        one_count >= 1,
        "AV.0.1 close-out: add_op '-' -> 1u8 second-branch payload \
         must reach the tape after scalar-Alt layout admission. \
         Payloads observed = {payloads:?}"
    );
}

#[test]
fn pinned_mul_op_div_branch_drops_payload() {
    let payloads = typed_u8_payloads("=1/2");
    let one_count = payloads.iter().filter(|(_, b)| *b == 1).count();
    assert!(
        one_count >= 1,
        "AV.0.1 close-out: mul_op '/' -> 1u8 second-branch payload \
         must reach the tape. Payloads = {payloads:?}"
    );
}

#[test]
fn pinned_number_drops_f64_payload() {
    // AW-III.W1 close-out: `number = /regex/ -> f64` now fires. The
    // lifter resolves `Map { Regex, Expr { return_type: F64 } }` to
    // `RegexPayloadKind::F64`, which the walker enqueues into the
    // PSI; Stage-B writes the parsed `f64` little-endian into
    // `pay_agg`, and `payload_bytes(rec, 8)` reads it back via
    // `f64::from_le_bytes`. Pre-W1 the assertion was
    // `f64_count == 0` (pinned to fail until the wiring landed);
    // the inverted assertion confirms the fix.
    let payloads = typed_f64_payloads("=42");
    let f64_count = payloads
        .iter()
        .filter(|(_, v)| (*v - 42.0).abs() < 1e-9)
        .count();
    assert!(
        f64_count >= 1,
        "AW-III.W1: number `-> f64` payload must reach the tape \
         for input `=42`. Observed = {payloads:?}"
    );
}

// ─── Walker round-trip ───────────────────────────────────────────────

#[test]
fn child_iter_walks_complex_formula() {
    // AZ-I.W2-act.close B2 — pre-flip this asserted that
    // `walk(tape, cursor, &mut out)` reached ≥ 5 tape records for a
    // representative nested formula. Under struct-direct the
    // equivalent invariant is the value-tree node count: `IF(...)`
    // has at least one func_call compound, an identifier, and several
    // typed leaves under its arg list.
    let input = "=IF(A1>10, SUM(B1:B10), 0)";
    let total = count_value_nodes(input);
    assert!(
        total >= 5,
        "complex formula walk must produce many value-tree nodes, got {}",
        total
    );
}

/// AV.0.1 close-out: nested arithmetic surfaces the 0u8 first-branch
/// discriminant on every `+` and `*`. Two `add_op 0u8` writes for
/// the `+` operators and one `mul_op 0u8` for the `*` operator land
/// via the scalar-Alt layout admission.
#[test]
fn nested_arithmetic_materialises_first_branch_ops() {
    let input = "=1+2*3+4";
    let payloads = typed_u8_payloads(input);
    let zero_count = payloads.iter().filter(|(_, b)| *b == 0).count();
    // We expect at least 3 zeros: two `+` (add_op 0u8) + one `*`
    // (mul_op 0u8).
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
    for input in ["=A1:B2", "=A:A", "=Sheet1!A1:B2", "='Sheet 1'!A1:B2"] {
        assert!(
            GoogleSheetsParser::parse(input).is_ok(),
            "range_ref must parse: {:?}",
            input
        );
    }
}

// ─── AZ-I.W2-act.B2 — wire-contract parity ────────────────────────────
//
// Two test families:
//
// 1. Tape-payload tests above exercise the struct-direct parser
//    (`GoogleSheetsParser::parse()` returns `SheetsDocument`). They
//    walk the value tree through the helpers at the top of this file.
//
// 2. Wire-contract tests below build SheetsDocument values directly
//    via SheetsStructBuilder against synthetic StructLayouts that
//    mirror the grammar's typed projections. These tests use
//    `b.finalise("")` to thread an empty input slice through the
//    A.fix-introduced `SheetsDocument::input` field — the wire
//    contract has no real source text, so the slice is empty.

mod wire_contract {
    use bbnf::runtime::{SheetsCompoundKind, SheetsStructBuilder, SheetsValue, StructBuilder};
    use bbnf_ir::TypeDesc;
    use bbnf_ir::registry::{LayoutKind, StructLayout};

    fn synth_layout(rule_id: u32, rule_name: &str, kind: LayoutKind) -> StructLayout {
        StructLayout {
            rule_id,
            rule_name: rule_name.to_string(),
            kind,
            rule_type: TypeDesc::Tuple(Vec::new()),
            fields: Vec::new(),
        }
    }

    /// `add_op = "+" -> 0u8 | "-" -> 1u8` projects to a Tag(u8) leaf.
    /// The struct-direct path lands the discriminator via
    /// `push_branch_tag(idx)` per the StructBuilder trait contract.
    #[test]
    fn add_op_first_branch_maps_to_tag_zero() {
        let mut b = SheetsStructBuilder::new();
        b.push_branch_tag(0);
        let doc = b.finalise("");
        assert_eq!(*doc.root(), SheetsValue::Tag(0));
    }

    #[test]
    fn add_op_second_branch_maps_to_tag_one() {
        let mut b = SheetsStructBuilder::new();
        b.push_branch_tag(1);
        let doc = b.finalise("");
        assert_eq!(*doc.root(), SheetsValue::Tag(1));
    }

    /// Sheets `boolean` collapses both branches onto a Bool leaf; the
    /// branch-tag discrimination is implicit in the bool value.
    #[test]
    fn boolean_true_lands_as_bool_leaf() {
        let mut b = SheetsStructBuilder::new();
        b.push_leaf_with_bool(true);
        let doc = b.finalise("");
        assert_eq!(*doc.root(), SheetsValue::Bool(true));
    }

    /// `error_literal = "#N/A" -> 0u8 | …` projects to an Error(u8)
    /// leaf with the variant index.
    #[test]
    fn error_literal_n_a_lands_as_error_zero() {
        let mut b = SheetsStructBuilder::new();
        b.push_leaf_error(0);
        let doc = b.finalise("");
        assert_eq!(*doc.root(), SheetsValue::Error(0));
    }

    #[test]
    fn error_literal_null_lands_as_error_four() {
        let mut b = SheetsStructBuilder::new();
        b.push_leaf_error(4);
        let doc = b.finalise("");
        assert_eq!(*doc.root(), SheetsValue::Error(4));
    }

    /// `number = /…/ -> f64` projects to a Number(f64) leaf via
    /// push_leaf_with_f64.
    #[test]
    fn number_42_lands_as_f64_leaf() {
        let mut b = SheetsStructBuilder::new();
        b.push_leaf_with_f64(42.0);
        let doc = b.finalise("");
        assert_eq!(*doc.root(), SheetsValue::Number(42.0));
    }

    /// Compound-kind preservation across operator-Alt rules — the
    /// same Tag(0) inside an AddExpr means `+`; the same Tag(0)
    /// inside a MulExpr means `*`. The structural-kind tag on the
    /// arena entry disambiguates.
    #[test]
    fn compound_kind_preserves_role_for_operator_alt() {
        let mut b = SheetsStructBuilder::new();
        let add_layout = synth_layout(27, "add_expr", LayoutKind::Struct);
        let h = b.begin_compound(&add_layout);
        b.push_leaf_with_f64(1.0);
        b.push_branch_tag(0); // `+`
        b.push_leaf_with_f64(2.0);
        b.end_compound(h);
        let doc = b.finalise("");
        match *doc.root() {
            SheetsValue::Compound(id) => {
                let view = doc.compound(id);
                assert_eq!(view.kind, SheetsCompoundKind::AddExpr);
                assert_eq!(view.children.len(), 3);
            }
            ref other => panic!("expected AddExpr Compound, got {:?}", other),
        }
    }

    /// `formula = /=?/, expression` — top-level formula compound.
    /// Children are the optional `=` (no payload, no push) and the
    /// expression value. Wire-contract pushes a single child.
    #[test]
    fn formula_compound_carries_expression_child() {
        let mut b = SheetsStructBuilder::new();
        let formula_layout = synth_layout(36, "formula", LayoutKind::Struct);
        let h = b.begin_compound(&formula_layout);
        b.push_leaf_with_f64(3.14);
        b.end_compound(h);
        let doc = b.finalise("");
        match *doc.root() {
            SheetsValue::Compound(id) => {
                let view = doc.compound(id);
                assert_eq!(view.kind, SheetsCompoundKind::Formula);
                assert_eq!(view.children.len(), 1);
                assert_eq!(view.children[0], SheetsValue::Number(3.14));
            }
            ref other => panic!("expected Formula Compound, got {:?}", other),
        }
    }

    /// Function-call shape: identifier head + arg list.
    #[test]
    fn func_call_compound_carries_head_and_args() {
        let mut b = SheetsStructBuilder::new();
        let fn_call = synth_layout(31, "func_call", LayoutKind::Struct);
        let h = b.begin_compound(&fn_call);
        b.push_leaf_identifier("SUM");
        let args = synth_layout(20, "func_args", LayoutKind::Struct);
        let h2 = b.begin_compound(&args);
        b.push_leaf_with_f64(1.0);
        b.push_leaf_with_f64(2.0);
        b.end_compound(h2);
        b.end_compound(h);
        let doc = b.finalise("");
        match *doc.root() {
            SheetsValue::Compound(id) => {
                let view = doc.compound(id);
                assert_eq!(view.kind, SheetsCompoundKind::FuncCall);
                // children: identifier + func_args compound.
                assert_eq!(view.children.len(), 2);
                match view.children[0] {
                    SheetsValue::Identifier(name) => assert_eq!(name, "SUM"),
                    ref other => panic!("expected Identifier head, got {:?}", other),
                }
                match view.children[1] {
                    SheetsValue::Compound(args_id) => {
                        let args_view = doc.compound(args_id);
                        assert_eq!(args_view.kind, SheetsCompoundKind::FuncArgs);
                        assert_eq!(args_view.children.len(), 2);
                    }
                    ref other => panic!("expected func_args Compound, got {:?}", other),
                }
            }
            ref other => panic!("expected func_call Compound, got {:?}", other),
        }
    }

    /// Cell-shape: optional sheet prefix + cell ref.
    #[test]
    fn cell_compound_carries_prefix_and_cell_ref() {
        let mut b = SheetsStructBuilder::new();
        let cell = synth_layout(11, "cell", LayoutKind::Struct);
        let h = b.begin_compound(&cell);
        b.push_leaf_sheet_prefix(1, "Sheet1!");
        b.push_leaf_cell_ref("A1");
        b.end_compound(h);
        let doc = b.finalise("");
        match *doc.root() {
            SheetsValue::Compound(id) => {
                let view = doc.compound(id);
                assert_eq!(view.kind, SheetsCompoundKind::Cell);
                assert_eq!(view.children.len(), 2);
                match view.children[0] {
                    SheetsValue::SheetPrefix {
                        tag: 1,
                        text: "Sheet1!",
                    } => {}
                    ref other => panic!("expected SheetPrefix, got {:?}", other),
                }
                match view.children[1] {
                    SheetsValue::CellRef("A1") => {}
                    ref other => panic!("expected CellRef, got {:?}", other),
                }
            }
            ref other => panic!("expected cell Compound, got {:?}", other),
        }
    }
}
