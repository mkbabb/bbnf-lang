//! Typed-enum variant resolution for a [`PathSegment::VariantName`]
//! step.
//!
//! When the resolver lands on a tagged-enum [`StructLayout`] and the
//! current path step is a [`PathSegment::VariantName(name)`],
//! [`select_variant`] looks up the branch whose `StructField::name`
//! matches `name`. The match drives the `->` annotation in the
//! grammar source: a typed-enum branch projects its annotation as the
//! field name on the layout, so the path step's name aligns with
//! whatever the grammar author wrote on the right-hand side of `->`.
//!
//! Two error modes flow through:
//!
//! - No matching branch — [`PathErrorReason::UnknownVariant`].
//!   Alternatives carry every registered branch name on the layout.
//! - Two branches with the same name — [`PathErrorReason::AmbiguousVariant`].
//!   Grammar-author error; the resolver fails compilation with the
//!   duplicated name surfaced through the `alternatives` list so the
//!   diagnostic can name the offending branches.
//!
//! The resolver is layout-only — it does not consult the runtime tape
//! or the `TypeDesc` graph. Layout-side resolution is enough for the
//! W2.1 type-checker (which already routes tagged-enum variant steps
//! through identical logic at `check_path_against_registry`); this
//! module exposes the same lookup as a stand-alone surface so the W3
//! lazy executor and the W5 TS binding can re-use the resolver
//! verbatim without re-implementing the matching policy.

use bbnf_ir::registry::{LayoutKind, StructField, StructLayout};

use super::error::{PathError, PathErrorReason};

/// Resolve a variant-name step against a tagged-enum layout.
///
/// Returns the matched [`StructField`] on success; otherwise a
/// [`PathError`] keyed at the failing segment.
///
/// `segment_index` is the path-relative position of the failing
/// segment; the caller picks it up from the surrounding path-walker
/// and threads it through so the diagnostic anchors at the right
/// position.
pub fn select_variant<'r>(
    layout: &'r StructLayout,
    name: &str,
    segment_index: usize,
) -> Result<&'r StructField, PathError> {
    if !matches!(layout.kind, LayoutKind::TaggedEnum) {
        return Err(PathError::new(
            segment_index,
            format!("@{name}"),
            layout.rule_name.clone(),
            Vec::new(),
            PathErrorReason::UnknownVariant,
        ));
    }

    let mut matches = layout.branches().filter(|(_, f)| f.name == name);
    let first = matches.next();
    let extra = matches.next();

    match (first, extra) {
        (Some((_, field)), None) => Ok(field),
        (None, _) => Err(PathError::new(
            segment_index,
            format!("@{name}"),
            layout.rule_name.clone(),
            branch_names_static(layout),
            PathErrorReason::UnknownVariant,
        )),
        (Some(_), Some(_)) => Err(PathError::new(
            segment_index,
            format!("@{name}"),
            layout.rule_name.clone(),
            branch_names_static(layout),
            PathErrorReason::AmbiguousVariant,
        )),
    }
}

/// Layout-side enumeration of every branch name as `&'static str`.
/// Matches the leak discipline in [`crate::path::type_check`]: the
/// alternatives list is bounded by the per-error rendering, so a
/// `Box::leak` per unique alternative caps the leak at the build's
/// failing-path count.
fn branch_names_static(layout: &StructLayout) -> Vec<&'static str> {
    layout
        .branches()
        .map(|(_, f)| Box::leak(f.name.clone().into_boxed_str()) as &'static str)
        .collect()
}
