//! AZ-IV.W2.1 — positive fixtures for the typed-path compile-time
//! checker.
//!
//! Walks a synthetic [`StructRegistry`] populated with the canonical
//! shape of a JSON-style grammar and asserts
//! [`bbnf::path::check_path_against_registry`] resolves valid paths
//! into a `TypedPath<G, T>` without error.
//!
//! Ground covered:
//!
//! 1. Single-field path through a struct layout terminates in the
//!    field's `TypeDesc`.
//! 2. Indexed step into a `Vec<T>` field descends into the inner
//!    type.
//! 3. Tagged-enum variant selection via [`PathSegment::VariantName`]
//!    descends into the matching branch's payload.
//! 4. Wildcard step over a `Vec<T>` resolves identically to `Index`
//!    at the type level (the lazy-iter execution is W2.5).
//! 5. The borrowed-form `Path<'_>` entry point ([`bbnf::path::check_path`])
//!    composes with the slice form.

use bbnf::path::{
    Json, OwnedPathSegment, Path, PathSegment, TypedPath, check_path, check_path_against_registry,
    ir::IntoPathSegment,
};
use bbnf_ir::registry::{FieldSource, LayoutKind, StructField, StructLayout, StructRegistry};
use bbnf_ir::{StringId, TypeDesc};

const ROOT_RULE: &str = "Document";
const ROOT_NAME: StringId = 0;
const ARRAY_RULE: &str = "Array";
const ARRAY_NAME: StringId = 1;
const VALUE_RULE: &str = "Value";
const VALUE_NAME: StringId = 2;

/// Build a synthetic registry mimicking a small JSON-style grammar:
///
/// ```text
/// Document = name: Span, items: Vec<Array>
/// Array    = elements: Vec<Value>
/// Value    = TaggedEnum( string: Span | number: F64 | flag: Bool )
/// ```
///
/// `Named(_)` references carry [`StringId`] handles the registry does
/// not own (the strings table lives on `GrammarIR` in production).
/// The offline checker handles the missing strings table by treating
/// `Named` as transparent transit until a registered layout absorbs
/// the descent — sufficient for the W2.1 positive fixtures.
fn build_registry() -> StructRegistry {
    let mut r = StructRegistry::new();

    // Document — Struct with two fields: `name` (Span), `items`
    // (Vec<Array>).
    r.insert(StructLayout {
        rule_id: 0,
        rule_name: ROOT_RULE.to_string(),
        kind: LayoutKind::Struct,
        rule_type: TypeDesc::Tuple(vec![
            TypeDesc::Span,
            TypeDesc::Vec(Box::new(TypeDesc::Named(ARRAY_NAME))),
        ]),
        fields: vec![
            StructField {
                name: "name".to_string(),
                type_desc: TypeDesc::Span,
                source: FieldSource::SeqPosition { position: 0 },
            },
            StructField {
                name: "items".to_string(),
                type_desc: TypeDesc::Vec(Box::new(TypeDesc::Named(ARRAY_NAME))),
                source: FieldSource::SeqPosition { position: 1 },
            },
        ],
    });

    // Array — Struct with one RepeatElement field whose element type
    // is `Value`.
    r.insert(StructLayout {
        rule_id: 1,
        rule_name: ARRAY_RULE.to_string(),
        kind: LayoutKind::Struct,
        rule_type: TypeDesc::Vec(Box::new(TypeDesc::Named(VALUE_NAME))),
        fields: vec![StructField {
            name: "element".to_string(),
            type_desc: TypeDesc::Named(VALUE_NAME),
            source: FieldSource::RepeatElement,
        }],
    });

    // Value — TaggedEnum with three branches.
    r.insert(StructLayout {
        rule_id: 2,
        rule_name: VALUE_RULE.to_string(),
        kind: LayoutKind::TaggedEnum,
        rule_type: TypeDesc::BoxedEnum,
        fields: vec![
            StructField {
                name: "string".to_string(),
                type_desc: TypeDesc::Span,
                source: FieldSource::BranchTag { branch_index: 0 },
            },
            StructField {
                name: "number".to_string(),
                type_desc: TypeDesc::F64,
                source: FieldSource::BranchTag { branch_index: 1 },
            },
            StructField {
                name: "flag".to_string(),
                type_desc: TypeDesc::Bool,
                source: FieldSource::BranchTag { branch_index: 2 },
            },
        ],
    });

    let _ = ROOT_NAME; // re-affirm the StringId symmetry above.
    r
}

#[test]
fn single_field_resolves_to_typed_path() {
    let registry = build_registry();
    let segments: [PathSegment<'_>; 1] = ["name".into_path_segment()];
    let resolved: TypedPath<Json, ()> =
        check_path_against_registry(&segments, &registry, ROOT_RULE)
            .expect("`name` must resolve against `Document`");
    assert_eq!(resolved.len(), 1);
    let owned = resolved.owned_segments();
    assert!(matches!(&owned[0], OwnedPathSegment::Field(s) if s == "name"));
}

#[test]
fn indexed_step_through_vec_field_resolves() {
    let registry = build_registry();
    let segments: [PathSegment<'_>; 2] = ["items".into_path_segment(), 0usize.into_path_segment()];
    let resolved: TypedPath<Json, ()> =
        check_path_against_registry(&segments, &registry, ROOT_RULE)
            .expect("`items[0]` must resolve against `Document`");
    assert_eq!(resolved.len(), 2);
    assert!(matches!(
        resolved.owned_segments()[1],
        OwnedPathSegment::Index(0)
    ));
}

#[test]
fn variant_name_step_resolves_against_tagged_enum() {
    let registry = build_registry();
    let segments: [PathSegment<'_>; 1] = [PathSegment::VariantName("string")];
    let resolved: TypedPath<Json, ()> =
        check_path_against_registry(&segments, &registry, VALUE_RULE)
            .expect("`@string` must select the matching branch on `Value`");
    assert_eq!(resolved.len(), 1);
    let owned = &resolved.owned_segments()[0];
    assert_eq!(owned.variant_name(), Some("string"));
}

#[test]
fn wildcard_step_resolves_through_array_repeat_element() {
    let registry = build_registry();
    let segments: [PathSegment<'_>; 1] = [PathSegment::Wildcard];
    let resolved: TypedPath<Json, ()> =
        check_path_against_registry(&segments, &registry, ARRAY_RULE)
            .expect("`*` must resolve over `Array`'s repeat-element field");
    assert_eq!(resolved.len(), 1);
    assert!(matches!(
        resolved.owned_segments()[0],
        OwnedPathSegment::Wildcard
    ));
}

#[test]
fn check_path_borrowed_entry_composes_with_slice_entry() {
    let registry = build_registry();
    let segments: [PathSegment<'_>; 2] = ["items".into_path_segment(), 0usize.into_path_segment()];
    let path = Path::new(&segments);

    let via_slice: TypedPath<Json, ()> =
        check_path_against_registry(path.as_slice(), &registry, ROOT_RULE)
            .expect("slice form must resolve");
    let via_borrow: TypedPath<Json, ()> =
        check_path(path, &registry, ROOT_RULE).expect("borrow form must resolve");

    assert_eq!(via_slice.len(), via_borrow.len());
    assert_eq!(via_slice.owned_segments(), via_borrow.owned_segments());
}

#[test]
fn typed_path_segments_round_trip_through_owned_form() {
    let registry = build_registry();
    let segments: [PathSegment<'_>; 2] = ["items".into_path_segment(), PathSegment::Wildcard];
    let resolved: TypedPath<Json, ()> =
        check_path_against_registry(&segments, &registry, ROOT_RULE)
            .expect("`items.*` must resolve");

    let projected = resolved.segments();
    assert_eq!(projected.len(), 2);
    assert!(matches!(projected[0], PathSegment::Field("items")));
    assert!(matches!(projected[1], PathSegment::Wildcard));
}
