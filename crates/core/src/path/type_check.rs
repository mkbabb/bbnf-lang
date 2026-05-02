//! Offline path type-checker.
//!
//! [`check_path_against_registry`] walks a borrowed [`Path`] against a
//! `StructRegistry` produced by the IR's `project_types` closure. It
//! does not execute against parsed input; it only validates each
//! segment's reachability under the layout graph and constructs a
//! [`TypedPath`] when every segment resolves.
//!
//! The checker is consumed by:
//!
//! - The `path!` proc-macro at compile time. The macro reads the
//!   grammar's serialised registry, calls this function, and
//!   converts a [`PathError`] into a `proc_macro2::Span`-anchored
//!   `syn::Error`.
//! - The IR `path_check` pass (W2.2) when the inline-trace sidecar
//!   resolves a source-rule-named path against the post-pipeline
//!   registry.
//! - Tests in `crates/core/tests/path_typed_compile.rs` and
//!   `crates/core/tests/path_type_errors.rs` exercising the offline
//!   surface directly.
//!
//! Resolution rules per segment kind:
//!
//! - `Field(name)` — current layout must be a `Struct` /
//!   `NewtypeWrapper` / `UntaggedEnum`; the named field must exist.
//!   Resolution descends into the field's `TypeDesc`. Scalar payloads
//!   reject `Field` access ([`PathErrorReason::FieldOnScalar`]).
//! - `Index(_)` — the current type must be a `Vec<_>`; resolution
//!   descends into the inner type. Other types reject the step
//!   ([`PathErrorReason::IndexIntoNonList`]).
//! - `VariantName(name)` — current layout must be a `TaggedEnum`; the
//!   branch with that name (matched by `StructField::name`) is
//!   selected; resolution descends into the branch's `TypeDesc`.
//!   Missing match → [`PathErrorReason::UnknownVariant`]; multiple
//!   matches → [`PathErrorReason::AmbiguousVariant`].
//! - `Wildcard` — the current type must be a `Vec<_>`; resolution
//!   descends into the inner type, identical to `Index(_)` at the
//!   type level. Wildcard execution semantics (lazy iteration)
//!   arrive in W2.5; W2.1 only validates reach.

use bbnf_ir::{LayoutKind, StructLayout, StructRegistry, TypeDesc};

use super::error::{PathError, PathErrorReason};
use super::ir::{Path, PathSegment, TypedPath};

/// Resolve a borrowed [`Path`] against a `StructRegistry`.
///
/// `entry_rule` names the rule the path starts at (the grammar's
/// document root, typically). The function walks each segment,
/// descending through layouts and `TypeDesc` wrappers, and produces a
/// [`TypedPath<G, T>`] when every segment resolves. Caller picks `G`
/// and `T`; the macro emits the right pair after this function
/// validates the segments against the registry.
///
/// Returns [`PathError`] on the first failing segment. The error
/// carries the segment index, rendered text, the layout / struct name
/// the resolver was inside at the failure point, and the alternatives
/// the resolver would have accepted.
pub fn check_path_against_registry<G, T>(
    path: &[PathSegment<'_>],
    registry: &StructRegistry,
    entry_rule: &str,
) -> Result<TypedPath<G, T>, PathError> {
    if path.is_empty() {
        return Err(PathError::new(
            0,
            "<empty>",
            entry_rule,
            registered_rule_names(registry),
            PathErrorReason::EmptyPath,
        ));
    }

    let entry_layout = registry.layout_by_name(entry_rule).ok_or_else(|| {
        PathError::new(
            0,
            entry_rule,
            entry_rule,
            registered_rule_names(registry),
            PathErrorReason::UnregisteredRule,
        )
    })?;

    let mut cursor = ResolveCursor::Layout(entry_layout);

    for (idx, segment) in path.iter().enumerate() {
        cursor = step(cursor, segment, idx, registry)?;
    }

    Ok(TypedPath::from_segments(path))
}

/// Cursor over the layout graph during path resolution.
///
/// At each segment the resolver is positioned either at a registered
/// `StructLayout` (a `Named` rule's projection) or at a raw `TypeDesc`
/// produced by descending into a field. The two share the resolution
/// step but differ in the alternatives a failure surfaces.
#[derive(Debug, Clone, Copy)]
enum ResolveCursor<'a> {
    Layout(&'a StructLayout),
    Type(&'a TypeDesc, &'a str),
}

/// Apply one segment to the cursor; produce the next cursor or a
/// `PathError`.
fn step<'r>(
    cursor: ResolveCursor<'r>,
    segment: &PathSegment<'_>,
    segment_index: usize,
    registry: &'r StructRegistry,
) -> Result<ResolveCursor<'r>, PathError> {
    match cursor {
        ResolveCursor::Layout(layout) => step_into_layout(layout, segment, segment_index),
        ResolveCursor::Type(ty, parent) => {
            step_into_type(ty, parent, segment, segment_index, registry)
        }
    }
}

fn step_into_layout<'r>(
    layout: &'r StructLayout,
    segment: &PathSegment<'_>,
    segment_index: usize,
) -> Result<ResolveCursor<'r>, PathError> {
    match segment {
        PathSegment::Field(name) => match layout.kind {
            LayoutKind::TaggedEnum => Err(PathError::new(
                segment_index,
                segment.to_string(),
                layout.rule_name.clone(),
                layout
                    .branches()
                    .filter_map(|(_, f)| static_str_or_none(&f.name))
                    .collect(),
                // A bare `Field(name)` step on a tagged enum must
                // first select a variant; the resolver could not pick
                // the variant on its own.
                PathErrorReason::UnknownVariant,
            )),
            _ => match layout.field(name) {
                Some(field) => descend_type(&field.type_desc, layout.rule_name.as_str()),
                None => Err(PathError::new(
                    segment_index,
                    segment.to_string(),
                    layout.rule_name.clone(),
                    layout
                        .fields()
                        .filter_map(|f| static_str_or_none(&f.name))
                        .collect(),
                    PathErrorReason::UnknownField,
                )),
            },
        },
        PathSegment::Index(_) => {
            // A layout with a single `RepeatElement` field projects
            // `Vec<T>` at the rule's outermost type. Descend into the
            // element type if the layout shape supports it.
            if let Some(element) = layout.fields.iter().find(|f| f.is_repeat_element()) {
                descend_type(&element.type_desc, layout.rule_name.as_str())
            } else if let TypeDesc::Vec(inner) = &layout.rule_type {
                descend_type(inner, layout.rule_name.as_str())
            } else {
                Err(PathError::new(
                    segment_index,
                    segment.to_string(),
                    layout.rule_name.clone(),
                    Vec::new(),
                    PathErrorReason::IndexIntoNonList,
                ))
            }
        }
        PathSegment::Wildcard => {
            // Wildcard at type level resolves identically to Index;
            // the lazy-iter execution lane (W2.5) keeps the resolved
            // element type and emits an `Iter<Item = T>` adapter.
            if let Some(element) = layout.fields.iter().find(|f| f.is_repeat_element()) {
                descend_type(&element.type_desc, layout.rule_name.as_str())
            } else if let TypeDesc::Vec(inner) = &layout.rule_type {
                descend_type(inner, layout.rule_name.as_str())
            } else {
                Err(PathError::new(
                    segment_index,
                    segment.to_string(),
                    layout.rule_name.clone(),
                    Vec::new(),
                    PathErrorReason::IndexIntoNonList,
                ))
            }
        }
        PathSegment::VariantName(name) => {
            if !matches!(layout.kind, LayoutKind::TaggedEnum) {
                return Err(PathError::new(
                    segment_index,
                    segment.to_string(),
                    layout.rule_name.clone(),
                    Vec::new(),
                    PathErrorReason::UnknownVariant,
                ));
            }
            let mut matches = layout.branches().filter(|(_, f)| f.name == *name);
            let first = matches.next();
            let extra = matches.next();
            match (first, extra) {
                (Some((_, field)), None) => {
                    descend_type(&field.type_desc, layout.rule_name.as_str())
                }
                (None, _) => Err(PathError::new(
                    segment_index,
                    segment.to_string(),
                    layout.rule_name.clone(),
                    layout
                        .branches()
                        .filter_map(|(_, f)| static_str_or_none(&f.name))
                        .collect(),
                    PathErrorReason::UnknownVariant,
                )),
                (Some(_), Some(_)) => Err(PathError::new(
                    segment_index,
                    segment.to_string(),
                    layout.rule_name.clone(),
                    layout
                        .branches()
                        .filter_map(|(_, f)| static_str_or_none(&f.name))
                        .collect(),
                    PathErrorReason::AmbiguousVariant,
                )),
            }
        }
    }
}

fn step_into_type<'r>(
    ty: &'r TypeDesc,
    parent: &'r str,
    segment: &PathSegment<'_>,
    segment_index: usize,
    registry: &'r StructRegistry,
) -> Result<ResolveCursor<'r>, PathError> {
    match (ty, segment) {
        (TypeDesc::Vec(inner), PathSegment::Index(_)) => descend_type(inner, parent),
        (TypeDesc::Vec(inner), PathSegment::Wildcard) => descend_type(inner, parent),
        (TypeDesc::Option(inner), seg) => {
            // Transparent: stepping through an `Option<T>` collapses
            // to stepping through `T`. The proc-macro carries the
            // optionality at the terminal type by wrapping the final
            // `T` in `Option<_>` if any layer was an `Option`.
            step_into_type(inner, parent, seg, segment_index, registry)
        }
        (TypeDesc::Named(_), _) => {
            // `Named` reached a layout boundary that the descend step
            // should have unfolded; reaching it as a `ResolveCursor::Type`
            // means descend_type left it intact (no registry entry).
            Err(PathError::new(
                segment_index,
                segment.to_string(),
                parent.to_owned(),
                registered_rule_names(registry),
                PathErrorReason::UnregisteredRule,
            ))
        }
        (scalar, _) if scalar.is_scalar_payload() => Err(PathError::new(
            segment_index,
            segment.to_string(),
            parent.to_owned(),
            Vec::new(),
            PathErrorReason::FieldOnScalar,
        )),
        (_, PathSegment::Index(_)) | (_, PathSegment::Wildcard) => Err(PathError::new(
            segment_index,
            segment.to_string(),
            parent.to_owned(),
            Vec::new(),
            PathErrorReason::IndexIntoNonList,
        )),
        // Any other (Type, Field/Variant) pair lands here: the type
        // is neither a list, scalar, named-rule, nor option, so there
        // is no traversal step that lifts the segment.
        _ => Err(PathError::new(
            segment_index,
            segment.to_string(),
            parent.to_owned(),
            Vec::new(),
            PathErrorReason::UnknownField,
        )),
    }
}

/// Descend a `TypeDesc` into a `ResolveCursor` for the next iteration.
///
/// Leaves `Vec<_>` / `Option<_>` / scalars / `Named(_)` as
/// `ResolveCursor::Type`. The grammar-side parent name is carried so
/// failures in the next step name the correct surrounding scope.
///
/// `Named(_)` resolution is intentionally deferred: the registry is
/// keyed by `RuleId` while `TypeDesc::Named` carries a `StringId`, and
/// the strings table that produced that handle lives on `GrammarIR`,
/// outside the offline-checker entry point. The IR `path_check` pass
/// (W2.2) holds the strings table at call site and does the real
/// `Named(_)` → layout lookup. The offline checker leaves the descent
/// at `ResolveCursor::Type(Named(_), parent)`; if a follow-up segment
/// then needs to descend, it surfaces [`PathErrorReason::UnregisteredRule`]
/// with the registered rule names as alternatives — exactly what W2.4's
/// macro consumer needs to produce a useful diagnostic.
fn descend_type<'r>(ty: &'r TypeDesc, parent: &'r str) -> Result<ResolveCursor<'r>, PathError> {
    Ok(ResolveCursor::Type(ty, parent))
}

/// Best-effort `&'static str` projection of a registered field name
/// for diagnostics. Field names live as owned `String`s on the
/// layout; `PathError::alternatives` is `Vec<&'static str>` so the
/// proc-macro can interpolate without ownership concerns. We leak
/// once per unique alternative through `Box::leak`; alternatives are
/// per-error and field counts are small (< 30 per layout), so the
/// leak is bounded by the number of *failing* paths in a build.
fn static_str_or_none(s: &str) -> Option<&'static str> {
    Some(Box::leak(s.to_owned().into_boxed_str()) as &'static str)
}

/// Return every registered rule name as `&'static str`s for inclusion
/// in a `PathError::alternatives` list. Same leak discipline as
/// [`static_str_or_none`].
fn registered_rule_names(registry: &StructRegistry) -> Vec<&'static str> {
    registry
        .iter()
        .filter_map(|l| static_str_or_none(l.rule_name.as_str()))
        .collect()
}

/// Convenience entry point that walks a [`Path`] (the borrowed wrapper
/// around a segment slice) instead of a raw slice.
pub fn check_path<G, T>(
    path: Path<'_>,
    registry: &StructRegistry,
    entry_rule: &str,
) -> Result<TypedPath<G, T>, PathError> {
    check_path_against_registry(path.as_slice(), registry, entry_rule)
}
