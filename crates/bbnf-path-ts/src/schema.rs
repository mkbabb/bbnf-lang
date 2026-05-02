//! Serializable shape mirrors of the Rust frontend's path types.
//!
//! Each payload here is byte-identical (under `serde-wasm-bindgen`) to
//! its `bbnf::path::*` counterpart. The isomorphism is asserted in
//! `tests/isomorphic_path_error.rs`.
//!
//! - [`TypedPathPayload`] mirrors `bbnf::path::TypedPath::owned_segments()`
//!   plus a `grammar` marker tag.
//! - [`OwnedSegmentPayload`] mirrors `bbnf::path::OwnedPathSegment`.
//! - [`PathErrorPayload`] mirrors `bbnf::path::PathError` field-for-field.
//! - [`PathErrorReasonPayload`] mirrors `bbnf::path::PathErrorReason`.
//!
//! The TS frontend at `ts/path.ts` carries a TypeScript interface for
//! every shape here; the W5.2 Node-execute proof asserts the round
//! trip through `serde-wasm-bindgen`.

use serde::{Deserialize, Serialize};

/// Owned path segment — TS-bridge mirror of `bbnf::path::OwnedPathSegment`.
///
/// Variants tagged via `serde(tag = "kind")` so the JS object shape is
/// `{ kind: "Field", value: "statuses" }` etc., matching the
/// JSON-projection form a TS consumer expects.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
#[serde(tag = "kind", content = "value")]
pub enum OwnedSegmentPayload {
    /// Borrow-by-name field step.
    Field(String),
    /// Positional list / tuple index.
    Index(usize),
    /// Wildcard step (matches every element).
    Wildcard,
    /// Variant-name selector for typed-enum branches.
    VariantName(String),
}

/// Typed path payload — TS-bridge mirror of
/// `bbnf::path::TypedPath::<G, ()>::from_owned(...)`.
///
/// `grammar` carries the marker name (`"Json"`, `"CssL4"`, …) the
/// proc-macro reads from the marker ZST's trailing identifier; the TS
/// template-tag passes the same string when calling
/// [`super::compile_path`].
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct TypedPathPayload {
    /// Grammar marker name (e.g. `"Json"`).
    pub grammar: String,
    /// Owned segment sequence — left-to-right.
    pub segments: Vec<OwnedSegmentPayload>,
}

/// Path error reason — TS-bridge mirror of
/// `bbnf::path::PathErrorReason`. Discriminants are `serde(rename)`'d
/// to the Rust enum's variant names so the TS-side JS object reads
/// `{ reason: "UnknownField", … }` and the round trip through
/// `serde-wasm-bindgen` deserializes back into the Rust
/// `PathErrorReason` enum byte-for-byte.
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum PathErrorReasonPayload {
    UnknownField,
    UnknownVariant,
    IndexIntoNonList,
    FieldOnScalar,
    EmptyPath,
    UnregisteredRule,
    AmbiguousVariant,
    WildcardOverflow,
}

/// Path-resolution diagnostic — TS-bridge mirror of
/// `bbnf::path::PathError`.
///
/// Field-for-field identical to the Rust struct so the isomorphism
/// test at `tests/isomorphic_path_error.rs` round-trips a
/// `PathError` from Rust → JSON → `PathErrorPayload` → JSON →
/// `PathError` without loss. The `alternatives` field is `Vec<String>`
/// (vs the Rust `Vec<&'static str>`) only because static-borrow
/// lifetimes do not survive a serde round-trip; the contents are
/// identical.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct PathErrorPayload {
    /// Zero-based index of the failing segment in the input path.
    pub segment_index: usize,
    /// Rendered text of the failing segment.
    pub segment_str: String,
    /// Name of the struct / enum / rule the resolver was inside when
    /// the failure occurred.
    pub struct_name: String,
    /// Alternatives the resolver would have accepted at this position.
    pub alternatives: Vec<String>,
    /// Structured failure reason.
    pub reason: PathErrorReasonPayload,
}

impl PathErrorPayload {
    /// Construct with the canonical field set. Mirrors
    /// `bbnf::path::PathError::new`.
    pub fn new(
        segment_index: usize,
        segment_str: impl Into<String>,
        struct_name: impl Into<String>,
        alternatives: Vec<String>,
        reason: PathErrorReasonPayload,
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
