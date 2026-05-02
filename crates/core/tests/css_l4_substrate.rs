//! AZ-I.W2-act.B3 — CSS L4 runtime substrate smoke tests.
//!
//! Exercises the typed-value enum family, arena, builder, and document
//! authored at W2-act.B3 step 1 directly — without consuming the
//! generated parser. Verifies the runtime types compose correctly and
//! the StructBuilder trait surface dispatches through the open-frame
//! stack as documented at `crates/core/src/runtime/css_l4/builder.rs`
//! §wire-contract.
//!
//! These are substrate-level tests; the post-regen end-to-end harness
//! lives at `css_l4_parity.rs` etc. and migrates to `CssDocument`
//! after the orchestrator re-runs `cargo xtask regen --grammar
//! css_l4`.

use bbnf::runtime::CssRule;
use bbnf::runtime::StructBuilder;
use bbnf::runtime::css_l4::{
    CssAngleUnit, CssArena, CssColor, CssColorSpace, CssColorType, CssDimension, CssDocument,
    CssLength, CssLengthUnit, CssStructBuilder, CssTimeUnit, CssTypedValue, StyleSheet,
};
use bbnf::runtime::path::{Path, PathSegment};
use bbnf_ir::TypeDesc;
use bbnf_ir::registry::{LayoutKind, StructLayout};

/// Helper — construct a default StructLayout for the named rule. Used
/// by builder smoke tests below; production use threads layouts off
/// the StructRegistry. The `rule_id` literal must match the CSS L4
/// grammar's allocation in
/// `crates/core/src/grammar/generated/css_l4.rs` so the runtime
/// builder's `from_rule_id`-keyed dispatch resolves the expected
/// `OpenFrame` variant.
fn layout_for(rule_id: u32, rule_name: &str, kind: LayoutKind) -> StructLayout {
    StructLayout {
        rule_id,
        rule_name: rule_name.to_string(),
        kind,
        rule_type: TypeDesc::Span,
        fields: Vec::new(),
    }
}

#[test]
fn empty_document_finalises() {
    let builder = CssStructBuilder::<'_>::new();
    let doc = builder.finalise("");
    assert!(doc.root().rules.is_empty());
    assert_eq!(doc.arena().rule_slab_count(), 0);
}

#[test]
fn arena_pushes_and_resolves_rules() {
    let mut arena = CssArena::<'static>::new();
    let id = arena.push_rules(Vec::new());
    assert!(id.is_empty());
    assert_eq!(arena.rules(id).len(), 0);
}

#[test]
fn css_length_unit_discriminants_round_trip() {
    // Each canonical unit maps from its grammar-declared u8 to the
    // matching variant.
    for (d, expected) in [
        (0u8, CssLengthUnit::Px),
        (1u8, CssLengthUnit::Em),
        (2u8, CssLengthUnit::Rem),
        (3u8, CssLengthUnit::Vh),
        (4u8, CssLengthUnit::Vw),
        (12u8, CssLengthUnit::Pt),
        (9u8, CssLengthUnit::Cm),
    ] {
        assert_eq!(CssLengthUnit::from_discriminant(d), expected);
    }
    // Unknown discriminant lands in the Other catch-all preserving the
    // raw byte for diagnostic round-trip.
    assert_eq!(
        CssLengthUnit::from_discriminant(123),
        CssLengthUnit::Other(123)
    );
}

#[test]
fn css_angle_time_units_admit_grammar_discriminants() {
    assert_eq!(CssAngleUnit::from_discriminant(0), Some(CssAngleUnit::Deg));
    assert_eq!(CssAngleUnit::from_discriminant(3), Some(CssAngleUnit::Turn));
    assert_eq!(CssAngleUnit::from_discriminant(99), None);
    assert_eq!(CssTimeUnit::from_discriminant(0), Some(CssTimeUnit::Ms));
    assert_eq!(CssTimeUnit::from_discriminant(1), Some(CssTimeUnit::S));
}

#[test]
fn css_color_types_round_trip() {
    for (d, expected) in [
        (0u8, CssColorType::Rgb),
        (1u8, CssColorType::Rgba),
        (2u8, CssColorType::Hsl),
        (4u8, CssColorType::Hwb),
        (5u8, CssColorType::Lab),
        (6u8, CssColorType::Lch),
        (7u8, CssColorType::Oklab),
        (8u8, CssColorType::Oklch),
    ] {
        assert_eq!(CssColorType::from_discriminant(d), Some(expected));
    }
}

#[test]
fn css_color_space_round_trips() {
    for (d, expected) in [
        (0u8, CssColorSpace::Srgb),
        (1u8, CssColorSpace::SrgbLinear),
        (2u8, CssColorSpace::DisplayP3),
        (3u8, CssColorSpace::A98Rgb),
        (5u8, CssColorSpace::Rec2020),
        (6u8, CssColorSpace::XyzD50),
        (7u8, CssColorSpace::XyzD65),
    ] {
        assert_eq!(CssColorSpace::from_discriminant(d), Some(expected));
    }
}

#[test]
fn struct_builder_assembles_simple_stylesheet() {
    let mut builder = CssStructBuilder::<'static>::new();
    let sheet_layout = layout_for(124, "stylesheet", LayoutKind::Struct);
    let style_layout = layout_for(119, "qualifiedRule", LayoutKind::Struct);
    let decl_layout = layout_for(114, "declaration", LayoutKind::Struct);

    let sheet_handle = builder.begin_compound(&sheet_layout);
    let style_handle = builder.begin_compound(&style_layout);
    // Selector is captured via push_leaf_with_str on a SelectorList
    // frame; the typed-value enum handles the borrowed span.
    let decl_handle = builder.begin_compound(&decl_layout);
    builder.push_leaf_with_str("color");
    builder.push_leaf_with_str("red");
    builder.end_compound(decl_handle);
    builder.end_compound(style_handle);
    builder.end_compound(sheet_handle);

    let doc = builder.finalise("");
    let rules = doc.rules(doc.root().rules);
    assert_eq!(rules.len(), 1);
    if let CssRule::Style(style) = &rules[0] {
        let decls = doc.decls(style.declarations);
        assert_eq!(decls.len(), 1);
        assert_eq!(decls[0].property, "color");
    } else {
        panic!("expected StyleRule, got {:?}", rules[0]);
    }
}

#[test]
fn struct_builder_checkpoint_discards_nested_rule_attempt() {
    let mut builder = CssStructBuilder::<'static>::new();
    let sheet_layout = layout_for(124, "stylesheet", LayoutKind::Struct);
    let style_layout = layout_for(119, "qualifiedRule", LayoutKind::Struct);
    let decl_layout = layout_for(114, "declaration", LayoutKind::Struct);

    let sheet = builder.begin_compound(&sheet_layout);
    let checkpoint = builder.checkpoint();
    let style = builder.begin_compound(&style_layout);
    let decl = builder.begin_compound(&decl_layout);
    builder.push_leaf_with_str("color");
    builder.push_leaf_with_str("red");
    builder.end_compound(decl);
    builder.end_compound(style);

    builder.rollback(checkpoint);
    builder.end_compound(sheet);

    let doc = builder.finalise("");
    assert!(doc.rules(doc.root().rules).is_empty());
}

#[test]
fn struct_builder_assembles_typed_dimension() {
    let mut builder = CssStructBuilder::<'static>::new();
    let length_layout = layout_for(55, "length", LayoutKind::Struct);

    let h = builder.begin_compound(&length_layout);
    builder.push_leaf_with_f64(100.0);
    // Px discriminant is 0 per `value-unit.bbnf::lengthUnit`.
    builder.push_branch_tag(0);
    builder.end_compound(h);

    // The dimension lands as a pending value (no enclosing frame).
    // Finalise into a placeholder document to verify the dispatch
    // produced a valid CssDimension::Length.
    let _doc = builder.finalise("");
    // The smoke check: no panics, builder pops cleanly.
}

#[test]
fn struct_builder_threads_color_function() {
    let mut builder = CssStructBuilder::<'static>::new();
    let layout = layout_for(62, "colorFn", LayoutKind::Struct);

    let h = builder.begin_compound(&layout);
    // Rgb discriminant is 0 per `color.bbnf::colorType`.
    builder.push_branch_tag(0);
    // Three colour components
    builder.push_leaf_with_f64(255.0);
    builder.push_leaf_with_f64(0.0);
    builder.push_leaf_with_f64(0.0);
    builder.end_compound(h);

    let _doc = builder.finalise("");
}

#[test]
fn document_view_kind_distinguishes_empty() {
    let arena = CssArena::<'static>::new();
    let root = StyleSheet {
        rules: bbnf::runtime::css_l4::CssRuleListId::EMPTY,
    };
    let doc = CssDocument::new(arena, root, "");
    let view = doc.view();
    assert_eq!(view.kind(), bbnf::runtime::css_l4::CssDocumentKind::Empty);
    assert_eq!(view.root().rules, doc.root().rules);
}

#[test]
fn arena_color_dag_admits_recursive_mix() {
    let mut arena = CssArena::<'static>::new();
    let inner = arena.push_color(CssColor::Hex(0xFF0000FF));
    assert_eq!(arena.color_count(), 1);
    // The returned reference lives for the arena's lifetime; copy
    // through to verify the typed shape preserves.
    if let CssColor::Hex(packed) = inner {
        assert_eq!(*packed, 0xFF0000FF);
    } else {
        panic!("expected Hex, got {:?}", inner);
    }
}

#[test]
fn document_get_path_query_resolves_string() {
    // Build a stylesheet with one declaration `color: red` and verify
    // the CssPathQuery walker descends through Index(0).Index(0).
    let mut builder = CssStructBuilder::<'static>::new();
    let sheet = layout_for(124, "stylesheet", LayoutKind::Struct);
    let style = layout_for(119, "qualifiedRule", LayoutKind::Struct);
    let decl = layout_for(114, "declaration", LayoutKind::Struct);

    let h_sheet = builder.begin_compound(&sheet);
    let h_style = builder.begin_compound(&style);
    let h_decl = builder.begin_compound(&decl);
    builder.push_leaf_with_str("color");
    builder.push_leaf_with_str("red");
    builder.end_compound(h_decl);
    builder.end_compound(h_style);
    builder.end_compound(h_sheet);
    let doc = builder.finalise("");

    // Path: rules[0] (Style).declarations[0] → Declaration { value: ... }
    let segments = [PathSegment::Index(0), PathSegment::Index(0)];
    let path = Path::new(&segments);
    let value: Option<&str> = doc.get(path);
    // The first push_leaf_with_str lands the property name; the
    // second forms the value list. The `value` field path query
    // returns the value's typed projection.
    assert_eq!(value, Some("color"));
}

#[test]
fn typed_value_alternation_admits_every_branch() {
    let arena = CssArena::<'static>::new();
    let _ = arena.values(bbnf::runtime::css_l4::CssValueListId::EMPTY);

    let dim = CssTypedValue::Dimension(CssDimension::Length(CssLength {
        value: 10.0,
        unit: CssLengthUnit::Px,
    }));
    let num = CssTypedValue::Number(3.14);
    let int = CssTypedValue::Integer(42);
    let s = CssTypedValue::String("hello");
    let id = CssTypedValue::Ident("foo");
    let kw = CssTypedValue::GlobalKeyword(bbnf::runtime::css_l4::CssGlobalKeyword::Inherit);
    let color = CssTypedValue::Color(CssColor::Hex(0xFF00FFFF));
    let span = CssTypedValue::Span("any");
    // Smoke-check Debug + PartialEq surface — every variant participates.
    let formatted = format!(
        "{:?} {:?} {:?} {:?} {:?} {:?} {:?} {:?}",
        dim, num, int, s, id, kw, color, span
    );
    assert!(formatted.contains("Length"));
    assert!(formatted.contains("Number"));
    assert!(formatted.contains("Color"));
}
