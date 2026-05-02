//! AZ-IV.W2.1 — negative fixtures for the typed-path compile-time
//! checker.
//!
//! Confirms [`bbnf::path::check_path_against_registry`] returns a
//! [`PathError`] naming the offending segment and the alternatives the
//! resolver would have accepted.
//!
//! Coverage:
//!
//! 1. Unknown field on a struct surfaces
//!    [`PathErrorReason::UnknownField`] with field-name alternatives.
//! 2. Index step into a non-list type surfaces
//!    [`PathErrorReason::IndexIntoNonList`].
//! 3. Variant-name step against a non-tagged-enum layout surfaces
//!    [`PathErrorReason::UnknownVariant`].
//! 4. Unknown variant name on a tagged-enum surfaces
//!    [`PathErrorReason::UnknownVariant`] with branch-name alternatives.
//! 5. Empty path surfaces [`PathErrorReason::EmptyPath`].
//! 6. Unregistered entry rule surfaces [`PathErrorReason::UnregisteredRule`].
//! 7. Ambiguous variant (two branches share a name — grammar-author
//!    error) surfaces [`PathErrorReason::AmbiguousVariant`].

use bbnf::path::{
    Json, PathError, PathErrorReason, PathSegment, TypedPath, check_path_against_registry,
    ir::IntoPathSegment,
};
use bbnf_ir::TypeDesc;
use bbnf_ir::registry::{FieldSource, LayoutKind, StructField, StructLayout, StructRegistry};

const DOC_RULE: &str = "Document";
const ENUM_RULE: &str = "Choice";

fn registry_with_one_struct_and_one_enum() -> StructRegistry {
    let mut r = StructRegistry::new();

    // Document — Struct with two fields.
    r.insert(StructLayout {
        rule_id: 0,
        rule_name: DOC_RULE.to_string(),
        kind: LayoutKind::Struct,
        rule_type: TypeDesc::Tuple(vec![TypeDesc::Span, TypeDesc::F64]),
        fields: vec![
            StructField {
                name: "title".to_string(),
                type_desc: TypeDesc::Span,
                source: FieldSource::SeqPosition { position: 0 },
            },
            StructField {
                name: "score".to_string(),
                type_desc: TypeDesc::F64,
                source: FieldSource::SeqPosition { position: 1 },
            },
        ],
    });

    // Choice — TaggedEnum with two distinct branches.
    r.insert(StructLayout {
        rule_id: 1,
        rule_name: ENUM_RULE.to_string(),
        kind: LayoutKind::TaggedEnum,
        rule_type: TypeDesc::BoxedEnum,
        fields: vec![
            StructField {
                name: "left".to_string(),
                type_desc: TypeDesc::Span,
                source: FieldSource::BranchTag { branch_index: 0 },
            },
            StructField {
                name: "right".to_string(),
                type_desc: TypeDesc::F64,
                source: FieldSource::BranchTag { branch_index: 1 },
            },
        ],
    });

    r
}

fn registry_with_ambiguous_enum() -> StructRegistry {
    let mut r = StructRegistry::new();
    r.insert(StructLayout {
        rule_id: 0,
        rule_name: ENUM_RULE.to_string(),
        kind: LayoutKind::TaggedEnum,
        rule_type: TypeDesc::BoxedEnum,
        fields: vec![
            StructField {
                name: "shared".to_string(),
                type_desc: TypeDesc::Span,
                source: FieldSource::BranchTag { branch_index: 0 },
            },
            StructField {
                name: "shared".to_string(),
                type_desc: TypeDesc::F64,
                source: FieldSource::BranchTag { branch_index: 1 },
            },
        ],
    });
    r
}

fn check_for_error(
    segments: &[PathSegment<'_>],
    registry: &StructRegistry,
    entry: &str,
) -> PathError {
    let result: Result<TypedPath<Json, ()>, PathError> =
        check_path_against_registry(segments, registry, entry);
    match result {
        Err(e) => e,
        Ok(_) => panic!("expected PathError for {segments:?} on `{entry}`"),
    }
}

#[test]
fn unknown_field_names_segment_and_alternatives() {
    let registry = registry_with_one_struct_and_one_enum();
    let segments: [PathSegment<'_>; 1] = ["nope".into_path_segment()];
    let err = check_for_error(&segments, &registry, DOC_RULE);

    assert_eq!(err.reason, PathErrorReason::UnknownField);
    assert_eq!(err.segment_index, 0);
    assert_eq!(err.segment_str, "nope");
    assert_eq!(err.struct_name, DOC_RULE);
    let alts: Vec<&str> = err.alternatives.iter().copied().collect();
    assert!(
        alts.contains(&"title") && alts.contains(&"score"),
        "alternatives must include the registered fields: got {alts:?}"
    );
}

#[test]
fn index_into_non_list_is_diagnosed() {
    let registry = registry_with_one_struct_and_one_enum();
    // `Document.title` is `Span` — indexing it must fail. The error
    // surfaces at segment_index 1 because segment 0 (`title`) is
    // valid and descends to a `Span` cursor; segment 1 (`Index(0)`)
    // is the failing step.
    let segments: [PathSegment<'_>; 2] = ["title".into_path_segment(), 0usize.into_path_segment()];
    let err = check_for_error(&segments, &registry, DOC_RULE);

    assert!(matches!(
        err.reason,
        PathErrorReason::IndexIntoNonList | PathErrorReason::FieldOnScalar
    ));
    assert_eq!(err.segment_index, 1);
}

#[test]
fn variant_name_against_struct_is_diagnosed() {
    let registry = registry_with_one_struct_and_one_enum();
    let segments: [PathSegment<'_>; 1] = [PathSegment::VariantName("title")];
    let err = check_for_error(&segments, &registry, DOC_RULE);

    assert_eq!(err.reason, PathErrorReason::UnknownVariant);
    assert_eq!(err.segment_index, 0);
    assert_eq!(err.struct_name, DOC_RULE);
}

#[test]
fn unknown_variant_lists_branch_alternatives() {
    let registry = registry_with_one_struct_and_one_enum();
    let segments: [PathSegment<'_>; 1] = [PathSegment::VariantName("middle")];
    let err = check_for_error(&segments, &registry, ENUM_RULE);

    assert_eq!(err.reason, PathErrorReason::UnknownVariant);
    assert_eq!(err.segment_str, "@middle");
    assert_eq!(err.struct_name, ENUM_RULE);
    let alts: Vec<&str> = err.alternatives.iter().copied().collect();
    assert!(
        alts.contains(&"left") && alts.contains(&"right"),
        "alternatives must include both branch names: got {alts:?}"
    );
}

#[test]
fn empty_path_is_diagnosed() {
    let registry = registry_with_one_struct_and_one_enum();
    let err = check_for_error(&[], &registry, DOC_RULE);

    assert_eq!(err.reason, PathErrorReason::EmptyPath);
    assert_eq!(err.segment_index, 0);
}

#[test]
fn unregistered_entry_rule_is_diagnosed() {
    let registry = registry_with_one_struct_and_one_enum();
    let segments: [PathSegment<'_>; 1] = ["title".into_path_segment()];
    let err = check_for_error(&segments, &registry, "MissingRule");

    assert_eq!(err.reason, PathErrorReason::UnregisteredRule);
    let alts: Vec<&str> = err.alternatives.iter().copied().collect();
    assert!(
        alts.contains(&DOC_RULE) && alts.contains(&ENUM_RULE),
        "alternatives must enumerate every registered rule: got {alts:?}"
    );
}

#[test]
fn ambiguous_variant_surfaces_named_diagnostic() {
    let registry = registry_with_ambiguous_enum();
    let segments: [PathSegment<'_>; 1] = [PathSegment::VariantName("shared")];
    let err = check_for_error(&segments, &registry, ENUM_RULE);

    assert_eq!(err.reason, PathErrorReason::AmbiguousVariant);
    assert_eq!(err.segment_str, "@shared");
}

#[test]
fn diagnostic_display_renders_full_segment_chain() {
    let registry = registry_with_one_struct_and_one_enum();
    let segments: [PathSegment<'_>; 1] = ["nope".into_path_segment()];
    let err = check_for_error(&segments, &registry, DOC_RULE);

    let rendered = format!("{err}");
    assert!(
        rendered.contains("nope"),
        "diagnostic must name the segment: {rendered}"
    );
    assert!(
        rendered.contains(DOC_RULE),
        "diagnostic must name the struct: {rendered}"
    );
    assert!(
        rendered.contains("unknown field"),
        "diagnostic must name the failure mode: {rendered}"
    );
}
