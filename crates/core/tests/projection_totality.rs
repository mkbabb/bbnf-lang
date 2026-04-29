//! AZ-II.cutover.O3 P1 -- StructDirect document-owned projection proof.
//!
//! The pre-O3 test treated generated tape projection machinery as the
//! runtime witness. That surface retires with the generated-view purge:
//! StructDirect parse results are concrete runtime documents, and the
//! proof has to assert the document/value/accessor facts callers
//! actually consume.
//!
//! This file therefore avoids generated tape-view metadata entirely.
//! The runtime gate parses concrete fixtures and proves that values are
//! reachable through the owning `Document` plus its arena/path/walk
//! accessors.

mod common;

// CSS L4 generated code resolves `crate::css_types::parse_hex_color`.
// Re-export the test shim at this module root so the generated module
// keeps compiling without a generated compatibility surface.
#[allow(unused_imports)]
use common::css_types;

use bbnf::grammar::generated::bbnf::BbnfBootstrap;
use bbnf::grammar::generated::css_l4::CssL4Parser;
use bbnf::grammar::generated::google_sheets::GoogleSheetsParser;
use bbnf::grammar::generated::json::JsonParser;
use bbnf::runtime::{
    BbnfCompoundKind, BbnfValue, CssDimension, CssTypedValue, JsonValue, Path, PathSegment,
    SheetsCompoundKind, SheetsDocument, SheetsValue,
};

fn path<'a>(segments: &'a [PathSegment<'a>]) -> Path<'a> {
    Path::new(segments)
}

fn assert_nearly_eq(actual: f64, expected: f64, label: &str) {
    assert!(
        (actual - expected).abs() < 1e-9,
        "{label}: expected {expected}, got {actual}"
    );
}

fn walk_sheets_values<'p, F>(doc: &SheetsDocument<'p>, mut visit: F)
where
    F: FnMut(SheetsValue<'p>, SheetsCompoundKind),
{
    let mut stack = vec![(*doc.root(), SheetsCompoundKind::Wrap)];
    while let Some((value, parent_kind)) = stack.pop() {
        visit(value, parent_kind);
        if let SheetsValue::Compound(id) = value {
            let entry = doc.compound(id);
            for child in entry.children.iter().rev() {
                stack.push((*child, entry.kind));
            }
        }
    }
}

fn collect_bbnf_spans<'p>(doc: &bbnf::runtime::BbnfDocument<'p>) -> Vec<&'p str> {
    fn walk<'p>(
        doc: &bbnf::runtime::BbnfDocument<'p>,
        value: BbnfValue<'p>,
        out: &mut Vec<&'p str>,
    ) {
        match value {
            BbnfValue::Span(s) => out.push(s),
            BbnfValue::Compound(id) => {
                for child in &doc.compound(id).children {
                    walk(doc, *child, out);
                }
            }
            BbnfValue::Int(_)
            | BbnfValue::Float(_)
            | BbnfValue::Bool(_)
            | BbnfValue::Tag(_)
            | BbnfValue::Unit => {}
        }
    }

    let mut spans = Vec::new();
    walk(doc, *doc.root(), &mut spans);
    spans
}

#[test]
fn struct_direct_documents_expose_projection_values() {
    // JSON: object pairs and typed path access are owned by JsonDocument.
    let json_src = r#"{"title":"hello","count":2}"#;
    let json =
        JsonParser::parse(json_src).unwrap_or_else(|e| panic!("JsonParser: parse failed: {e:?}"));
    assert_eq!(json.input(), json_src);
    let JsonValue::Object(object_id) = *json.to_value() else {
        panic!(
            "JsonParser: expected object root, got {:?}",
            json.to_value()
        );
    };
    let pairs = json.object(object_id);
    assert_eq!(pairs.len(), 2, "JsonDocument object pair count");
    assert_eq!(pairs[0].key, "title");
    assert_eq!(pairs[1].key, "count");
    assert_eq!(
        json.get::<&str>(path(&[PathSegment::Field("title")])),
        Some("hello"),
        "JsonDocument field accessor preserves string payload"
    );
    assert_nearly_eq(
        json.get::<f64>(path(&[PathSegment::Field("count")]))
            .expect("JsonDocument number field"),
        2.0,
        "JsonDocument field accessor preserves numeric payload",
    );

    // CSS L4: stylesheet/declaration/value walks expose document-owned
    // value lists and dimension payloads from CssDocument's arena.
    let css_src = "a { color: rgb(255 128 0 / 0.5); width: 50%; }";
    let css =
        CssL4Parser::parse(css_src).unwrap_or_else(|e| panic!("CssL4Parser: parse failed: {e:?}"));
    assert_eq!(css.input(), css_src);
    assert!(
        !css.rules(css.root().rules).is_empty(),
        "CssDocument stylesheet should carry at least one top-level rule"
    );
    let declarations: Vec<_> = css.walk_declarations().collect();
    assert!(
        declarations.len() >= 2,
        "CssDocument declaration walk should reach the style block declarations, got {declarations:?}"
    );
    let color_list_id = css
        .walk_values()
        .find_map(|(_property, value)| match value {
            CssTypedValue::List(id) => Some(*id),
            _ => None,
        })
        .expect("CssDocument value walk should reach a color value list");
    let color_list = css.values(color_list_id);
    assert!(
        color_list
            .iter()
            .any(|value| matches!(value, CssTypedValue::Span(name) if *name == "rgb")),
        "CssDocument value-list accessor must expose the rgb() function head, got {color_list:?}"
    );
    let width_percentage = css.walk_values().any(|(_property, value)| {
        matches!(
            value,
            CssTypedValue::Dimension(CssDimension::Percentage(percentage))
                if (percentage.value - 50.0).abs() < 1e-9
        )
    });
    assert!(
        width_percentage,
        "CssDocument value walk must reach width: 50% as a typed percentage"
    );

    // Sheets: the formula document owns compound-kind and scalar
    // payload reachability; operator meaning comes from the enclosing
    // compound kind, not from a generated value enum.
    let sheets_src = "=1+2";
    let sheets = GoogleSheetsParser::parse(sheets_src)
        .unwrap_or_else(|e| panic!("GoogleSheetsParser: parse failed: {e:?}"));
    assert_eq!(sheets.input(), sheets_src);
    let SheetsValue::Compound(formula_id) = *sheets.to_value() else {
        panic!(
            "SheetsDocument: expected formula compound root, got {:?}",
            sheets.to_value()
        );
    };
    let formula = sheets.compound(formula_id);
    assert_eq!(formula.kind, SheetsCompoundKind::Formula);
    assert!(
        !formula.children.is_empty(),
        "formula must carry an expression child"
    );

    let mut numbers = Vec::new();
    let mut saw_add_expr = false;
    walk_sheets_values(&sheets, |value, parent_kind| match value {
        SheetsValue::Number(n) => numbers.push(n),
        SheetsValue::Compound(id) => {
            saw_add_expr |= sheets.compound(id).kind == SheetsCompoundKind::AddExpr;
        }
        SheetsValue::Tag(_) if parent_kind == SheetsCompoundKind::AddExpr => {
            saw_add_expr = true;
        }
        _ => {}
    });
    numbers.sort_by(|a, b| a.partial_cmp(b).unwrap());
    assert_eq!(numbers, vec![1.0, 2.0]);
    assert!(
        saw_add_expr,
        "SheetsDocument must preserve the additive expression node"
    );

    // BBNF: the grammar document exposes structural rule/alternation
    // nodes and borrowed spans via BbnfDocument/BbnfView.
    let bbnf_src = "foo = \"a\" | \"b\" ;\n";
    let bbnf = BbnfBootstrap::parse(bbnf_src)
        .unwrap_or_else(|e| panic!("BbnfBootstrap: parse failed: {e:?}"));
    assert_eq!(bbnf.input(), bbnf_src);
    let root_view = bbnf.view();
    assert_eq!(root_view.compound_kind(), Some(BbnfCompoundKind::Grammar));
    let rule_view = root_view
        .find_descendant_by_kind(BbnfCompoundKind::Rule)
        .expect("BbnfDocument rule descendant");
    assert!(
        rule_view.span_text().contains("foo"),
        "BbnfDocument rule span must cover the lhs identifier"
    );
    let alternation = root_view
        .find_descendant_by_kind(BbnfCompoundKind::Alternation)
        .expect("BbnfDocument alternation descendant");
    assert!(
        alternation.span_text().contains('|'),
        "BbnfDocument alternation span must cover the branch separator"
    );
    let spans = collect_bbnf_spans(&bbnf);
    assert!(
        spans.iter().any(|span| *span == "foo"),
        "BbnfDocument span walk must reach the lhs identifier"
    );
    assert!(
        spans.iter().any(|span| span.contains('a')),
        "BbnfDocument span walk must reach the first literal"
    );
}

#[test]
fn struct_direct_documents_have_concrete_roots() {
    let json = JsonParser::parse("\"ready\"").expect("JSON string parse");
    assert_eq!(*json.root(), JsonValue::String("ready"));

    let css = CssL4Parser::parse("a { width: 50%; }").expect("CSS L4 parse");
    let has_percentage = css.walk_values().any(|(_property, value)| {
        matches!(value, CssTypedValue::Dimension(CssDimension::Percentage(_)))
    });
    assert!(
        has_percentage,
        "CSS percentage must reach the typed document graph"
    );

    let sheets = GoogleSheetsParser::parse("=42").expect("Sheets number parse");
    let mut saw_number = false;
    walk_sheets_values(&sheets, |value, _parent| {
        saw_number |= matches!(value, SheetsValue::Number(n) if (n - 42.0).abs() < 1e-9);
    });
    assert!(
        saw_number,
        "Sheets number must reach the typed document graph"
    );

    let bbnf = BbnfBootstrap::parse("r = 'x' ;").expect("BBNF parse");
    assert_eq!(bbnf.view().compound_kind(), Some(BbnfCompoundKind::Grammar));
}
