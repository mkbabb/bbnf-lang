//! O3 runtime-root tests for StructDirect document-owned APIs.
//!
//! These tests intentionally avoid legacy wrapper, `Root`,
//! `TapeCursor`, generated `NodeView`, and `ValueRoot` surfaces.
//! StructDirect roots are proven here through concrete `*Document`
//! roots, typed view kinds,
//! arena-backed accessors, and the grammar-agnostic `RuntimeView`
//! traversal surface.

use ::bbnf::grammar::generated::bbnf::BbnfBootstrap;
use ::bbnf::grammar::generated::bnf::BnfParser;
use ::bbnf::grammar::generated::css_l4::CssL4Parser;
use ::bbnf::grammar::generated::css_pretty::CssPrettyParser;
use ::bbnf::grammar::generated::csv::CsvParser;
use ::bbnf::grammar::generated::ebnf::EbnfParser;
use ::bbnf::grammar::generated::google_sheets::GoogleSheetsParser;
use ::bbnf::grammar::generated::json::JsonParser;
use ::bbnf::grammar::generated::math::MathParser;
use bbnf::runtime::{
    BbnfCompoundKind, BbnfKind, BbnfValue, BnfCompoundKind, BnfKind, BnfValue, CssDocumentKind,
    CssPrettyCompoundKind, CssPrettyKind, CssPrettyValue, CssRule, CsvCompoundKind, CsvKind,
    CsvValue, EbnfCompoundKind, EbnfKind, EbnfValue, JsonKind, JsonValue, MathKind, MathValue,
    RuntimeView, SheetsCompoundKind, SheetsKind, SheetsValue,
};

#[test]
fn json_root_is_document_owned_object_view() {
    let input = "{\"a\":1,\"b\":true}";
    let doc = JsonParser::parse(input).expect("JSON parse");
    let view = doc.view();

    assert_eq!(doc.input(), input);
    assert!(std::ptr::eq(doc.to_value(), doc.root()));
    assert_eq!(view.kind(), JsonKind::Object);
    assert_eq!(RuntimeView::children(&view).count(), 2);

    let JsonValue::Object(root_id) = *doc.root() else {
        panic!("JSON root must be an object");
    };
    let pairs = doc.object(root_id);
    assert_eq!(pairs.len(), 2);
    assert_eq!(pairs[0].key, "a");
}

#[test]
fn css_l4_root_is_document_owned_stylesheet_view() {
    let input = "a { color: red; }";
    let doc = CssL4Parser::parse(input).expect("CSS L4 parse");
    let view = doc.view();

    assert_eq!(doc.input(), input);
    assert!(std::ptr::eq(doc.to_value(), doc.root()));
    assert_eq!(RuntimeView::kind(&view), CssDocumentKind::StyleSheet);

    let rules = doc.rules(doc.root().rules);
    assert_eq!(RuntimeView::children(&view).count(), rules.len());
    let style_rule = rules
        .iter()
        .find_map(|rule| match rule {
            CssRule::Style(style_rule) => Some(*style_rule),
            _ => None,
        })
        .expect("CSS L4 root must contain a style rule");
    let declarations = doc.decls(style_rule.declarations);
    assert!(
        !declarations.is_empty(),
        "CSS L4 style rule must expose document-owned declarations",
    );
    assert!(
        doc.walk_values().count() > 0,
        "CSS L4 document must expose typed values through document-owned walkers",
    );
}

#[test]
fn sheets_root_is_document_owned_formula_view() {
    let input = "=1+2";
    let doc = GoogleSheetsParser::parse(input).expect("Sheets parse");
    let view = doc.view();

    assert_eq!(doc.input(), input);
    assert!(std::ptr::eq(doc.to_value(), doc.root()));
    assert_eq!(view.kind(), SheetsKind::Compound);

    let SheetsValue::Compound(root_id) = *doc.root() else {
        panic!("Sheets root must be a compound");
    };
    let root = doc.compound(root_id);
    assert_eq!(root.kind, SheetsCompoundKind::Formula);
    assert_eq!(RuntimeView::children(&view).count(), root.children.len());
    assert!(!root.children.is_empty());
}

#[test]
fn bbnf_root_is_document_owned_grammar_view() {
    let input = "foo = \"a\" | \"b\" ;\n";
    let doc = BbnfBootstrap::parse(input).expect("BBNF parse");
    let view = doc.view();

    assert_eq!(doc.input(), input);
    assert!(std::ptr::eq(doc.to_value(), doc.root()));
    assert_eq!(view.kind(), BbnfKind::Compound);
    assert_eq!(view.compound_kind(), Some(BbnfCompoundKind::Grammar));

    let BbnfValue::Compound(root_id) = *doc.root() else {
        panic!("BBNF root must be a compound");
    };
    let root = doc.compound(root_id);
    assert_eq!(root.kind, BbnfCompoundKind::Grammar);
    assert_eq!(RuntimeView::children(&view).count(), root.children.len());
    assert!(!root.children.is_empty());
}

#[test]
fn csv_root_is_document_owned_csv_view() {
    let input = "a,b,c\n1,2,3";
    let doc = CsvParser::parse(input).expect("CSV parse");
    let view = doc.view();

    assert_eq!(doc.input(), input);
    assert!(std::ptr::eq(doc.to_value(), doc.root()));
    assert_eq!(view.kind(), CsvKind::Compound);

    let CsvValue::Compound(root_id) = *doc.root() else {
        panic!("CSV root must be a compound");
    };
    let root = doc.compound(root_id);
    assert_eq!(root.kind, CsvCompoundKind::Csv);
    assert_eq!(RuntimeView::children(&view).count(), root.children.len());
    assert!(!root.children.is_empty());
}

#[test]
fn math_root_is_document_owned_number_view() {
    let input = "42";
    let doc = MathParser::parse(input).expect("Math parse");
    let view = doc.view();

    assert_eq!(doc.input(), input);
    assert!(std::ptr::eq(doc.to_value(), doc.root()));
    assert_eq!(view.kind(), MathKind::Span);
    assert_eq!(RuntimeView::span(&view), Some("42"));

    let MathValue::Span(text) = *doc.root() else {
        panic!("Math root must be a span");
    };
    assert_eq!(text, "42");
    assert_eq!(RuntimeView::children(&view).count(), 0);
}

#[test]
fn bnf_root_is_document_owned_grammar_view() {
    let input = "<expr> ::= <term> | <expr> \"+\" <term>\n";
    let doc = BnfParser::parse(input).expect("BNF parse");
    let view = doc.view();

    assert_eq!(doc.input(), input);
    assert!(std::ptr::eq(doc.to_value(), doc.root()));
    assert_eq!(view.kind(), BnfKind::Compound);
    assert_eq!(view.compound_kind(), Some(BnfCompoundKind::Grammar));

    let BnfValue::Compound(root_id) = *doc.root() else {
        panic!("BNF root must be a compound");
    };
    let root = doc.compound(root_id);
    assert_eq!(root.kind, BnfCompoundKind::Grammar);
    assert_eq!(RuntimeView::children(&view).count(), root.children.len());
    assert!(!root.children.is_empty());
}

#[test]
fn ebnf_root_is_document_owned_grammar_view() {
    let input = "digit = \"0\" | \"1\" | \"2\" ;\n";
    let doc = EbnfParser::parse(input).expect("EBNF parse");
    let view = doc.view();

    assert_eq!(doc.input(), input);
    assert!(std::ptr::eq(doc.to_value(), doc.root()));
    assert_eq!(view.kind(), EbnfKind::Compound);
    assert_eq!(view.compound_kind(), Some(EbnfCompoundKind::Grammar));

    let EbnfValue::Compound(root_id) = *doc.root() else {
        panic!("EBNF root must be a compound");
    };
    let root = doc.compound(root_id);
    assert_eq!(root.kind, EbnfCompoundKind::Grammar);
    assert_eq!(RuntimeView::children(&view).count(), root.children.len());
    assert!(!root.children.is_empty());
}

#[test]
fn css_pretty_root_is_document_owned_stylesheet_view() {
    let input = "body { color: red; }";
    let doc = CssPrettyParser::parse(input).expect("CSS Pretty parse");
    let view = doc.view();

    assert_eq!(doc.input(), input);
    assert!(std::ptr::eq(doc.to_value(), doc.root()));
    assert_eq!(view.kind(), CssPrettyKind::Compound);
    assert_eq!(
        view.compound_kind(),
        Some(CssPrettyCompoundKind::Stylesheet)
    );

    let CssPrettyValue::Compound(root_id) = *doc.root() else {
        panic!("CSS Pretty root must be a compound");
    };
    let root = doc.compound(root_id);
    assert_eq!(root.kind, CssPrettyCompoundKind::Stylesheet);
    assert_eq!(RuntimeView::children(&view).count(), root.children.len());
    assert!(!root.children.is_empty());
}
