//! AZ-IV.W2.5 — fixtures for the typed-enum variant resolver.
//!
//! Walks a synthetic [`StructRegistry`] mimicking the
//! `CssTypedValue` shape (Color | Length | Selector) and asserts
//! [`bbnf::path::select_variant`] dispatches the right branch and
//! surfaces the right diagnostic on failure.
//!
//! Ground covered:
//!
//! 1. A matching variant returns the branch field.
//! 2. An unknown variant fails with
//!    [`PathErrorReason::UnknownVariant`] and surfaces every registered
//!    branch name as alternatives.
//! 3. A struct-kind layout (non-tagged-enum) rejects the variant step
//!    outright.
//! 4. Two branches with the same `->` annotation surface
//!    [`PathErrorReason::AmbiguousVariant`] — grammar-author error.

use bbnf::path::error::PathErrorReason;
use bbnf::path::select_variant;
use bbnf_ir::registry::{FieldSource, LayoutKind, StructField, StructLayout};
use bbnf_ir::{StringId, TypeDesc};

const CSS_VALUE_NAME: StringId = 100;
const CSS_COLOR_NAME: StringId = 101;
const CSS_LENGTH_NAME: StringId = 102;
const CSS_SELECTOR_NAME: StringId = 103;

fn css_typed_value_layout() -> StructLayout {
    StructLayout {
        rule_id: CSS_VALUE_NAME,
        rule_name: "CssTypedValue".to_string(),
        kind: LayoutKind::TaggedEnum,
        rule_type: TypeDesc::BoxedEnum,
        fields: vec![
            StructField {
                name: "color".to_string(),
                type_desc: TypeDesc::Named(CSS_COLOR_NAME),
                source: FieldSource::BranchTag { branch_index: 0 },
            },
            StructField {
                name: "length".to_string(),
                type_desc: TypeDesc::Named(CSS_LENGTH_NAME),
                source: FieldSource::BranchTag { branch_index: 1 },
            },
            StructField {
                name: "selector".to_string(),
                type_desc: TypeDesc::Named(CSS_SELECTOR_NAME),
                source: FieldSource::BranchTag { branch_index: 2 },
            },
        ],
    }
}

fn struct_layout_non_enum() -> StructLayout {
    StructLayout {
        rule_id: 200,
        rule_name: "Document".to_string(),
        kind: LayoutKind::Struct,
        rule_type: TypeDesc::Tuple(vec![TypeDesc::Span]),
        fields: vec![StructField {
            name: "title".to_string(),
            type_desc: TypeDesc::Span,
            source: FieldSource::SeqPosition { position: 0 },
        }],
    }
}

fn ambiguous_layout() -> StructLayout {
    // Two branches with the same `->` annotation — grammar-author
    // error. The resolver must surface AmbiguousVariant rather than
    // silently picking one.
    StructLayout {
        rule_id: 300,
        rule_name: "Pun".to_string(),
        kind: LayoutKind::TaggedEnum,
        rule_type: TypeDesc::BoxedEnum,
        fields: vec![
            StructField {
                name: "color".to_string(),
                type_desc: TypeDesc::Named(CSS_COLOR_NAME),
                source: FieldSource::BranchTag { branch_index: 0 },
            },
            StructField {
                name: "color".to_string(),
                type_desc: TypeDesc::Named(CSS_COLOR_NAME),
                source: FieldSource::BranchTag { branch_index: 1 },
            },
        ],
    }
}

#[test]
fn matching_variant_resolves_to_branch_field() {
    let layout = css_typed_value_layout();
    let field =
        select_variant(&layout, "color", 4).expect("`color` must resolve against `CssTypedValue`");
    assert_eq!(field.name, "color");
    assert_eq!(field.type_desc, TypeDesc::Named(CSS_COLOR_NAME));
    assert_eq!(field.branch_index(), Some(0));
}

#[test]
fn unknown_variant_fails_with_alternatives() {
    let layout = css_typed_value_layout();
    let err =
        select_variant(&layout, "border", 2).expect_err("`border` is not a registered branch");
    assert_eq!(err.reason, PathErrorReason::UnknownVariant);
    assert_eq!(err.segment_index, 2);
    assert_eq!(err.struct_name, "CssTypedValue");
    assert_eq!(err.segment_str, "@border");
    let alts: Vec<&str> = err.alternatives.iter().copied().collect();
    assert!(alts.contains(&"color"));
    assert!(alts.contains(&"length"));
    assert!(alts.contains(&"selector"));
}

#[test]
fn struct_layout_rejects_variant_step() {
    let layout = struct_layout_non_enum();
    let err = select_variant(&layout, "title", 0)
        .expect_err("non-tagged-enum layouts must reject `@variant`");
    assert_eq!(err.reason, PathErrorReason::UnknownVariant);
    assert_eq!(err.struct_name, "Document");
    assert!(err.alternatives.is_empty());
}

#[test]
fn ambiguous_variant_surfaces_grammar_author_error() {
    let layout = ambiguous_layout();
    let err = select_variant(&layout, "color", 7)
        .expect_err("two branches with the same name must surface AmbiguousVariant");
    assert_eq!(err.reason, PathErrorReason::AmbiguousVariant);
    assert_eq!(err.segment_index, 7);
    assert_eq!(err.struct_name, "Pun");
    assert_eq!(err.segment_str, "@color");
}
