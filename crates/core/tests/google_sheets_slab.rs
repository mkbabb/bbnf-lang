//! Integration tests for Google Sheets formula parsing.
//!
//! Two test families:
//!
//! 1. **Parse-execution** — exercise `GoogleSheetsParser::parse(input)`
//!    against representative formula corpora. Pre-orchestrator-regen
//!    these run against the existing tape-direct parser; post-regen
//!    the same parses route through the struct-direct emitter and
//!    return [`SheetsDocument`].
//!
//! 2. **Wire-contract** (AZ-I.W2-act.B2) — exercise
//!    [`SheetsStructBuilder`] against the
//!    [`bbnf::runtime::StructBuilder`] trait with synthetic layouts
//!    that mirror the `grammar/google-sheets/google-sheets.bbnf`
//!    shapes, then compare the resulting [`SheetsDocument`] against
//!    expected typed shapes. The wire-contract proves the substrate
//!    is wired before the regen lands; the parse-execution tests
//!    activate post-regen.

use ::bbnf::grammar::generated::google_sheets::*;
// AZ-I.W2-act.B2 — wire-contract substrate: SheetsStructBuilder +
// SheetsDocument target the orchestrator-regen flip; the wire-contract
// section below exercises the typed shape directly without a
// `GoogleSheetsParser::parse()` round-trip, proving the substrate is
// wired before the orchestrator regens the parser.
use bbnf::runtime::{
    SheetsCompoundKind, SheetsDocument, SheetsStructBuilder, SheetsValue, StructBuilder,
};
use bbnf_ir::TypeDesc;
use bbnf_ir::registry::{LayoutKind, StructLayout};

/// Convenience: synthesise a `StructLayout` for a Sheets named rule.
/// W2-act.B2's wire-contract tests build layouts ad-hoc; the live
/// emitter reads layouts from `GrammarIR::struct_registry` (populated
/// by W1's `project_types` closure).
fn synth_layout(rule_id: u32, rule_name: &str, kind: LayoutKind) -> StructLayout {
    StructLayout {
        rule_id,
        rule_name: rule_name.to_string(),
        kind,
        rule_type: TypeDesc::Tuple(Vec::new()),
        fields: Vec::new(),
    }
}

/// Parse a formula and assert success. The tape-first parser
/// rejects trailing garbage automatically, so parse success
/// collapses the old completeness assertion.
fn parse_formula(input: &str) {
    let parsed = GoogleSheetsParser::parse(input)
        .unwrap_or_else(|e| panic!("parse failed for input {input:?}: {e:?}"));
    let _root = parsed.view();
}

// ── Simple formulas ──────────────────────────────────────────────────

#[test]
fn parse_sum() {
    parse_formula("=SUM(A1:A10)");
}

#[test]
fn parse_if() {
    parse_formula("=IF(A1>0,A1,0)");
}

#[test]
fn parse_arithmetic() {
    parse_formula("=1+2*3");
}

// ── Nested formulas ──────────────────────────────────────────────────

#[test]
fn parse_let_nested() {
    parse_formula("=LET(x,1,y,2,x+y)");
}

#[test]
fn parse_index_match() {
    parse_formula("=INDEX(A1:C10,MATCH(\"x\",A1:A10,0),1)");
}

// ── Prettify ─────────────────────────────────────────────────────────
//
// The prettify side channel still uses the parse_that combinator
// shape (`Grammar::rule_prettify()` → `Parser<'a, Vec<FmtOp<'a>>>`),
// unchanged by the tape-first migration.

#[test]
fn prettify_simple_formula() {
    let parser = GoogleSheetsParser::formula_prettify();
    let result = parser.parse("=SUM(A1:A10)");
    assert!(result.is_some(), "prettify should succeed for =SUM(A1:A10)");
}

#[test]
fn prettify_nested_formula() {
    let config = pprint::Printer::new(80, 2, false);
    let parser = GoogleSheetsParser::formula_prettify();
    let ops = parser.parse("=LET(x,1,y,2,x+y)");
    assert!(ops.is_some(), "prettify should succeed for LET formula");
    let rendered = pprint::render(&ops.unwrap(), config);
    assert!(
        rendered.contains("LET"),
        "rendered output should contain LET: got '{rendered}'"
    );
}

#[test]
fn prettify_if_formula() {
    let config = pprint::Printer::new(80, 2, false);
    let parser = GoogleSheetsParser::formula_prettify();
    let ops = parser.parse("=IF(A1>0,A1,0)");
    assert!(ops.is_some(), "prettify should succeed for IF formula");
    let rendered = pprint::render(&ops.unwrap(), config);
    assert!(
        rendered.contains("IF"),
        "rendered output should contain IF: got '{rendered}'"
    );
}

#[test]
fn prettify_index_match_formula() {
    let config = pprint::Printer::new(80, 2, false);
    let parser = GoogleSheetsParser::formula_prettify();
    let ops = parser.parse("=INDEX(A1:C10,MATCH(\"x\",A1:A10,0),1)");
    assert!(
        ops.is_some(),
        "prettify should succeed for INDEX/MATCH formula"
    );
    let rendered = pprint::render(&ops.unwrap(), config);
    assert!(
        rendered.contains("INDEX"),
        "rendered output should contain INDEX: got '{rendered}'"
    );
    assert!(
        rendered.contains("MATCH"),
        "rendered output should contain MATCH: got '{rendered}'"
    );
}

// ── AZ-I.W2-act.B2 wire-contract ─────────────────────────────────────
//
// The wire-contract tests exercise the SheetsStructBuilder against the
// StructBuilder trait, then finalise into a SheetsDocument. They prove
// the substrate is wired through the builder + arena + document under
// the trait surface.

#[test]
fn wire_contract_number_is_f64_leaf() {
    let mut b = SheetsStructBuilder::new();
    b.push_leaf_with_f64(42.0);
    let doc: SheetsDocument<'_> = b.finalise("");
    match *doc.root() {
        SheetsValue::Number(n) => assert_eq!(n, 42.0),
        ref other => panic!("expected Number leaf, got {:?}", other),
    }
}

#[test]
fn wire_contract_bool_true_leaf() {
    let mut b = SheetsStructBuilder::new();
    b.push_leaf_with_bool(true);
    let doc = b.finalise("");
    match *doc.root() {
        SheetsValue::Bool(true) => {}
        ref other => panic!("expected Bool(true), got {:?}", other),
    }
}

#[test]
fn wire_contract_bool_false_leaf() {
    let mut b = SheetsStructBuilder::new();
    b.push_leaf_with_bool(false);
    let doc = b.finalise("");
    match *doc.root() {
        SheetsValue::Bool(false) => {}
        ref other => panic!("expected Bool(false), got {:?}", other),
    }
}

#[test]
fn wire_contract_string_leaf_borrows_input() {
    let s: &'static str = "hello";
    let mut b = SheetsStructBuilder::new();
    b.push_leaf_with_str(s);
    let doc = b.finalise("");
    match *doc.root() {
        SheetsValue::String(slice) => assert_eq!(slice, "hello"),
        ref other => panic!("expected String leaf, got {:?}", other),
    }
}

#[test]
fn wire_contract_cell_ref_specialised_leaf() {
    let mut b = SheetsStructBuilder::new();
    b.push_leaf_cell_ref("$B$2");
    let doc = b.finalise("");
    match *doc.root() {
        SheetsValue::CellRef(slice) => assert_eq!(slice, "$B$2"),
        ref other => panic!("expected CellRef leaf, got {:?}", other),
    }
}

#[test]
fn wire_contract_identifier_specialised_leaf() {
    let mut b = SheetsStructBuilder::new();
    b.push_leaf_identifier("SUM");
    let doc = b.finalise("");
    match *doc.root() {
        SheetsValue::Identifier(slice) => assert_eq!(slice, "SUM"),
        ref other => panic!("expected Identifier leaf, got {:?}", other),
    }
}

#[test]
fn wire_contract_error_literal_carries_tag() {
    let mut b = SheetsStructBuilder::new();
    b.push_leaf_error(2); // #REF!
    let doc = b.finalise("");
    match *doc.root() {
        SheetsValue::Error(2) => {}
        ref other => panic!("expected Error(2), got {:?}", other),
    }
}

#[test]
fn wire_contract_sheet_prefix_carries_tag_and_text() {
    let mut b = SheetsStructBuilder::new();
    b.push_leaf_sheet_prefix(0, "'Sheet 1'!");
    let doc = b.finalise("");
    match *doc.root() {
        SheetsValue::SheetPrefix { tag, text } => {
            assert_eq!(tag, 0);
            assert_eq!(text, "'Sheet 1'!");
        }
        ref other => panic!("expected SheetPrefix leaf, got {:?}", other),
    }
}

#[test]
fn wire_contract_compound_with_two_children() {
    // Build the equivalent of `formula = /=?/, expression` by hand:
    //   formula compound { Number(1.0), Number(2.0) }
    let mut b = SheetsStructBuilder::new();
    let formula_layout = synth_layout(0, "formula", LayoutKind::Struct);
    let h = b.begin_compound(&formula_layout);
    b.push_leaf_with_f64(1.0);
    b.push_leaf_with_f64(2.0);
    b.end_compound(h);
    let doc = b.finalise("");
    match *doc.root() {
        SheetsValue::Compound(id) => {
            let view = doc.compound(id);
            assert_eq!(view.kind, SheetsCompoundKind::Formula);
            assert_eq!(view.children.len(), 2);
            match (view.children[0], view.children[1]) {
                (SheetsValue::Number(1.0), SheetsValue::Number(2.0)) => {}
                other => panic!("compound children mismatch: {:?}", other),
            }
        }
        ref other => panic!("expected Compound, got {:?}", other),
    }
}

#[test]
fn wire_contract_branch_tag_deposits_as_tag() {
    // add_op = "+" -> 0u8 | "-" -> 1u8.
    let mut b = SheetsStructBuilder::new();
    b.push_branch_tag(1);
    let doc = b.finalise("");
    match *doc.root() {
        SheetsValue::Tag(1) => {}
        ref other => panic!("expected Tag(1), got {:?}", other),
    }
}

#[test]
fn wire_contract_wrap_with_one_child_collapses() {
    // Wrap-shape compounds (primary | range_end | cell_or_range) with
    // exactly one child should collapse to the child's value.
    let mut b = SheetsStructBuilder::new();
    let wrap_layout = synth_layout(0, "primary", LayoutKind::TaggedEnum);
    let h = b.begin_compound(&wrap_layout);
    b.push_leaf_with_f64(3.14);
    b.end_compound(h);
    let doc = b.finalise("");
    // Primary -> Wrap path: collapses to the single child.
    match *doc.root() {
        SheetsValue::Number(n) => assert!((n - 3.14).abs() < f64::EPSILON),
        ref other => panic!("expected Wrap-collapsed Number, got {:?}", other),
    }
}

#[test]
fn wire_contract_compound_kind_disambiguates_role() {
    // Two compounds with the same Tag(0) child but different kinds —
    // an AddExpr Tag(0) means `+`, a MulExpr Tag(0) means `*`.
    let mut b1 = SheetsStructBuilder::new();
    let add_layout = synth_layout(0, "add_expr", LayoutKind::Struct);
    let h = b1.begin_compound(&add_layout);
    b1.push_branch_tag(0);
    b1.end_compound(h);
    let doc1 = b1.finalise("");

    let mut b2 = SheetsStructBuilder::new();
    let mul_layout = synth_layout(0, "mul_expr", LayoutKind::Struct);
    let h = b2.begin_compound(&mul_layout);
    b2.push_branch_tag(0);
    b2.end_compound(h);
    let doc2 = b2.finalise("");

    match *doc1.root() {
        SheetsValue::Compound(id) => {
            assert_eq!(doc1.compound(id).kind, SheetsCompoundKind::AddExpr);
        }
        ref other => panic!("expected AddExpr Compound, got {:?}", other),
    }
    match *doc2.root() {
        SheetsValue::Compound(id) => {
            assert_eq!(doc2.compound(id).kind, SheetsCompoundKind::MulExpr);
        }
        ref other => panic!("expected MulExpr Compound, got {:?}", other),
    }
}

#[test]
fn wire_contract_view_kind_discriminates_root() {
    let mut b = SheetsStructBuilder::new();
    b.push_leaf_with_f64(1.0);
    let doc = b.finalise("");
    let view = doc.view();
    assert!(view.is_number());
    assert!(!view.is_string());
    assert!(!view.is_compound());
}

#[test]
fn wire_contract_to_value_borrows_root() {
    let mut b = SheetsStructBuilder::new();
    b.push_leaf_with_f64(42.5);
    let doc = b.finalise("");
    let v = doc.to_value();
    match *v {
        SheetsValue::Number(n) => assert!((n - 42.5).abs() < f64::EPSILON),
        ref other => panic!("expected Number from to_value, got {:?}", other),
    }
}

#[test]
fn wire_contract_get_path_resolves_compound_index() {
    // Build a compound with three numeric children, query each by
    // index via the path API.
    let mut b = SheetsStructBuilder::new();
    let layout = synth_layout(0, "func_args", LayoutKind::Struct);
    let h = b.begin_compound(&layout);
    b.push_leaf_with_f64(10.0);
    b.push_leaf_with_f64(20.0);
    b.push_leaf_with_f64(30.0);
    b.end_compound(h);
    let doc = b.finalise("");
    use bbnf::runtime::path::{Path, PathSegment};
    let p0 = [PathSegment::Index(0)];
    let p1 = [PathSegment::Index(1)];
    let p2 = [PathSegment::Index(2)];
    assert_eq!(doc.get::<f64>(Path::new(&p0)), Some(10.0));
    assert_eq!(doc.get::<f64>(Path::new(&p1)), Some(20.0));
    assert_eq!(doc.get::<f64>(Path::new(&p2)), Some(30.0));
    let p_oob = [PathSegment::Index(99)];
    assert_eq!(doc.get::<f64>(Path::new(&p_oob)), None);
}
