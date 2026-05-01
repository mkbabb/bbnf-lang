//! AZ-II.cutover.A — BBNF struct-builder substrate smoke tests.
//!
//! Verifies the [`BbnfStructBuilder`] / [`BbnfArena`] / [`BbnfDocument`]
//! / [`BbnfView`] surface compiles and round-trips a synthetic
//! parse-shaped sequence. Mirrors the JSON struct-builder substrate
//! verification in `crates/ir/tests/struct_registry.rs` —
//! cutover.B's regen will produce the actual generated parser that
//! exercises this surface end-to-end.

use bbnf::runtime::RuntimeView;
use bbnf::runtime::bbnf::{
    BbnfArena, BbnfCompound, BbnfCompoundKind, BbnfDocument, BbnfKind, BbnfStructBuilder, BbnfValue,
};
use bbnf::runtime::builder::StructBuilder;
use bbnf::runtime::handle::CompoundHandle;
use bbnf_ir::registry::{LayoutKind, StructLayout};

/// Construct a synthetic `StructLayout` for a named rule. Used by
/// the smoke tests to drive `begin_compound` without standing up a
/// full IR.
fn synth_layout(rule_name: &str) -> StructLayout {
    StructLayout {
        rule_id: 0,
        rule_name: rule_name.to_string(),
        kind: LayoutKind::Struct,
        fields: Vec::new(),
        rule_type: bbnf_ir::types::TypeDesc::Span,
    }
}

#[test]
fn bbnf_struct_builder_round_trips_a_span_leaf() {
    let mut builder = BbnfStructBuilder::new();
    builder.push_leaf_with_str("identifier_value");
    let doc = builder.finalise("identifier_value");
    match doc.root() {
        BbnfValue::Span(s) => assert_eq!(*s, "identifier_value"),
        other => panic!("expected Span root, got {other:?}"),
    }
}

#[test]
fn bbnf_struct_builder_round_trips_an_int_leaf() {
    let mut builder = BbnfStructBuilder::new();
    builder.push_leaf_with_i64(42);
    let doc = builder.finalise("42");
    assert_eq!(doc.root(), &BbnfValue::Int(42));
}

#[test]
fn bbnf_struct_builder_round_trips_a_bool_leaf() {
    let mut builder = BbnfStructBuilder::new();
    builder.push_leaf_with_bool(true);
    let doc = builder.finalise("true");
    assert_eq!(doc.root(), &BbnfValue::Bool(true));
}

#[test]
fn bbnf_struct_builder_round_trips_a_compound_with_three_children() {
    let mut builder = BbnfStructBuilder::new();
    let layout = synth_layout("rule");
    let _handle: CompoundHandle = builder.begin_compound(&layout);
    builder.push_leaf_with_str("lhs_name");
    builder.push_leaf_with_str("=");
    builder.push_leaf_with_str("rhs_body");
    builder.end_compound(_handle);
    let doc = builder.finalise("lhs_name = rhs_body ;");

    match doc.root() {
        BbnfValue::Compound(id) => {
            let compound = doc.compound(*id);
            assert_eq!(compound.kind, BbnfCompoundKind::Rule);
            assert_eq!(compound.children.len(), 3);
        }
        other => panic!("expected Compound root, got {other:?}"),
    }
}

#[test]
fn bbnf_struct_builder_branch_tag_records_alt_index() {
    let mut builder = BbnfStructBuilder::new();
    let layout = synth_layout("directive");
    let handle: CompoundHandle = builder.begin_compound(&layout);
    builder.push_branch_tag(2);
    builder.push_leaf_with_str("@import");
    builder.end_compound(handle);
    let doc = builder.finalise("@import");

    match doc.root() {
        BbnfValue::Compound(id) => {
            let compound = doc.compound(*id);
            assert_eq!(compound.branch_tag, Some(2));
            assert_eq!(compound.kind, BbnfCompoundKind::Directive);
        }
        other => panic!("expected Compound root, got {other:?}"),
    }
}

#[test]
fn bbnf_view_kind_classifies_leaves_and_compounds() {
    let mut arena: BbnfArena<'static> = BbnfArena::new();
    let id = arena.push_compound(BbnfCompound {
        kind: BbnfCompoundKind::Grammar,
        branch_tag: None,
        children: vec![
            BbnfValue::Span("rule_name"),
            BbnfValue::Int(7),
            BbnfValue::Bool(false),
        ],
    });
    let doc = BbnfDocument::new(arena, BbnfValue::Compound(id), "");
    let view = doc.view();
    assert_eq!(view.kind(), BbnfKind::Compound);
    assert!(view.is_compound());

    let children: Vec<_> = view.children().map(|v| v.kind()).collect();
    assert_eq!(
        children,
        vec![BbnfKind::Span, BbnfKind::Int, BbnfKind::Bool]
    );
}

#[test]
fn bbnf_view_span_returns_borrowed_input_slice() {
    let arena: BbnfArena<'static> = BbnfArena::new();
    let doc = BbnfDocument::new(arena, BbnfValue::Span("source_slice"), "source_slice");
    let view = doc.view();
    assert_eq!(view.span(), Some("source_slice"));
    assert_eq!(view.input(), "source_slice");
}

#[test]
fn bbnf_view_runtime_view_trait_compiles() {
    // Compile-time check that BbnfView implements RuntimeView<'p>.
    fn _assert_runtime_view<'a, 'p, V: RuntimeView<'p, Kind = BbnfKind>>(_v: &V) {}
    let arena: BbnfArena<'static> = BbnfArena::new();
    let doc = BbnfDocument::new(arena, BbnfValue::Unit, "");
    let view = doc.view();
    _assert_runtime_view(&view);
}
