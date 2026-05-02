//! Path-resolution diagnostic.
//!
//! [`PathError`] carries the offending segment, the struct type the
//! path resolved against at the failure point, and the alternatives
//! the resolver could have accepted. The `path!` proc-macro
//! (AZ-IV.W2.4) wraps this into a `proc_macro2::Span`-anchored
//! `syn::Error` so the diagnostic anchors at the source path-segment
//! the user wrote.
//!
//! `PathError` is also returned at runtime when a borrowed `Path` is
//! checked against a `StructRegistry` outside the macro
//! (offline/compile-time checker entry point at
//! [`super::type_check::check_path_against_registry`]).

use core::fmt;

/// Reason a path failed to resolve.
///
/// Each variant names the structural failure mode:
///
/// - [`PathErrorReason::UnknownField`] — a `Field(name)` step did not
///   match any field on the current layout. Alternatives carry the
///   field names registered on the layout.
/// - [`PathErrorReason::UnknownVariant`] — a `VariantName(name)` step
///   did not match any branch on the current tagged-enum layout.
///   Alternatives carry the registered branch / sub-variant names.
/// - [`PathErrorReason::IndexIntoNonList`] — an `Index(_)` step
///   targeted a layout that is not a list. Alternatives are empty.
/// - [`PathErrorReason::FieldOnScalar`] — a `Field(_)` step targeted a
///   scalar payload (`F64`, `Bool`, …) which has no fields.
///   Alternatives are empty.
/// - [`PathErrorReason::EmptyPath`] — caller passed zero segments and
///   the entry point requires at least one. Alternatives are empty.
/// - [`PathErrorReason::UnregisteredRule`] — a `Field(name)` step
///   resolved to a `TypeDesc::Named(_)` whose target rule has no
///   `StructLayout` in the registry. Alternatives carry the registered
///   rule names.
/// - [`PathErrorReason::AmbiguousVariant`] — a `VariantName(name)`
///   step matched two or more branches. Grammar-author error;
///   alternatives carry the duplicate branch names.
/// - [`PathErrorReason::WildcardOverflow`] — a `Wildcard` step would
///   exceed the configured depth cap. Alternatives are empty.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum PathErrorReason {
    /// `Field(name)` did not match any field on the current layout.
    UnknownField,
    /// `VariantName(name)` did not match any branch on the current
    /// tagged-enum layout.
    UnknownVariant,
    /// `Index(_)` targeted a layout that is not a list.
    IndexIntoNonList,
    /// `Field(_)` targeted a scalar payload.
    FieldOnScalar,
    /// Empty path passed to a checker that requires at least one segment.
    EmptyPath,
    /// `Field(name)` resolved to a `TypeDesc::Named` whose target rule
    /// is not registered. Indicates a registry / pipeline desync.
    UnregisteredRule,
    /// `VariantName(name)` matched two or more branches. Grammar-author
    /// error.
    AmbiguousVariant,
    /// `Wildcard` would exceed the configured depth cap.
    WildcardOverflow,
}

impl fmt::Display for PathErrorReason {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            PathErrorReason::UnknownField => "unknown field",
            PathErrorReason::UnknownVariant => "unknown variant",
            PathErrorReason::IndexIntoNonList => "index into non-list",
            PathErrorReason::FieldOnScalar => "field access on scalar",
            PathErrorReason::EmptyPath => "empty path",
            PathErrorReason::UnregisteredRule => "unregistered rule",
            PathErrorReason::AmbiguousVariant => "ambiguous variant",
            PathErrorReason::WildcardOverflow => "wildcard depth overflow",
        };
        f.write_str(s)
    }
}

/// Path-resolution diagnostic.
///
/// Carries enough context for the proc-macro to render a span-anchored
/// compiler error: the offending segment's index in the input slice,
/// its rendered text, the struct-name the path was resolving against
/// at the failure point, and the list of valid alternatives at that
/// position.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PathError {
    /// Zero-based index of the failing segment in the input path.
    pub segment_index: usize,
    /// Rendered text of the failing segment (`"text"`, `"0"`, `"*"`,
    /// `"@variant"`). Caller-friendly for diagnostic anchoring.
    pub segment_str: String,
    /// Name of the struct / enum / rule the resolver was inside when
    /// the failure occurred. For the entry point it is the entry
    /// rule's name; for nested failures it is the parent layout's
    /// name.
    pub struct_name: String,
    /// Alternatives the resolver would have accepted at this position.
    /// Empty for failure modes where alternatives are not meaningful
    /// (e.g. [`PathErrorReason::FieldOnScalar`]).
    pub alternatives: Vec<&'static str>,
    /// Structured failure reason. Drives diagnostic rendering and
    /// machine-readable consumers (proc-macro, IR `path_check` pass).
    pub reason: PathErrorReason,
}

impl PathError {
    /// Construct a `PathError` with the canonical field set.
    pub fn new(
        segment_index: usize,
        segment_str: impl Into<String>,
        struct_name: impl Into<String>,
        alternatives: Vec<&'static str>,
        reason: PathErrorReason,
    ) -> Self {
        Self {
            segment_index,
            segment_str: segment_str.into(),
            struct_name: struct_name.into(),
            alternatives,
            reason,
        }
    }
}

impl fmt::Display for PathError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "path[{}] `{}` on `{}`: {}",
            self.segment_index, self.segment_str, self.struct_name, self.reason
        )?;
        if !self.alternatives.is_empty() {
            write!(f, "; valid: {:?}", self.alternatives)?;
        }
        Ok(())
    }
}

impl std::error::Error for PathError {}
