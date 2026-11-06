//! `StructRegistry` — grammar-derived native struct shapes per Named rule.
//!
//! `StructRegistry` is the substrate AZ-I.W1 lands so that JSON, CSS L4, and
//! Sheets emitters can write directly into typed Rust structs instead of
//! traversing an intermediate tape record stream. Every `Named` rule in the
//! grammar projects to a [`StructLayout`] describing the rule's emitted
//! struct shape: a discriminator [`LayoutKind`] (struct, tagged enum,
//! untagged enum, newtype wrapper) plus a list of [`StructField`]s whose
//! types and origins are derived from the rule's `IrNode` body and its
//! `project_types` closure.
//!
//! The registry is populated by an extension of [`crate::passes::project_types`]
//! (see [`crate::passes::types::populate_struct_registry`]). Each rule is
//! classified by its body shape: `Alt` → tagged enum (one variant per
//! alternation branch); `Seq` → struct with a field per position; `Map` of
//! a `Regex`/`Literal` with a typed `->` annotation → newtype wrapper; a
//! `Repeat` whose inner is itself typed → list-typed field. Recursive
//! references project as fields whose type is `TypeDesc::Named` keyed by the
//! referenced rule's name.
//!
//! The registry is consumed by:
//!
//! - The audit pass at [`crate::passes::audit::payload_coverage`] via the
//!   [`crate::passes::audit::StructRegistryProbe`] trait — once W1 closure
//!   has run, every typed `->` marker in JSON / CSS L4 / Sheets reaches a
//!   field on its enclosing rule's layout, projecting as `Mapped`.
//! - The Rust emitter (W2 / W3 stages) for direct struct-builder calls in
//!   place of tape records.
//!
//! Layout shape is exposed as data via accessor methods, not match arms in
//! the consumer; per `feedback_pluggable-components`, layout-kind
//! discrimination is queried, never duplicated at the consumer.

pub mod strategy;
#[allow(clippy::module_inception)]
mod r#struct;

/// Exact structural identifier for one alternative within an `Alt` node.
///
/// Semantic grammar values are not branch identifiers: typed `u8` maps travel
/// through the value API instead. The `u16` domain admits 65,536 alternatives
/// while fitting the shared 16-byte tape header without a side allocation.
#[derive(
    serde::Serialize, serde::Deserialize, Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord,
)]
#[serde(transparent)]
#[repr(transparent)]
pub struct AltBranchId(u16);

impl AltBranchId {
    pub const MAX_INDEX: u16 = u16::MAX;
    pub const MAX_COUNT: usize = u16::MAX as usize + 1;

    #[inline]
    pub const fn new(index: u16) -> Self {
        Self(index)
    }

    #[inline]
    pub const fn get(self) -> u16 {
        self.0
    }
}

impl From<u16> for AltBranchId {
    #[inline]
    fn from(index: u16) -> Self {
        Self::new(index)
    }
}

impl From<AltBranchId> for u16 {
    #[inline]
    fn from(index: AltBranchId) -> Self {
        index.get()
    }
}

impl From<AltBranchId> for u32 {
    #[inline]
    fn from(index: AltBranchId) -> Self {
        u32::from(index.get())
    }
}

impl TryFrom<u32> for AltBranchId {
    type Error = core::num::TryFromIntError;

    #[inline]
    fn try_from(index: u32) -> Result<Self, Self::Error> {
        u16::try_from(index).map(Self::new)
    }
}

impl TryFrom<usize> for AltBranchId {
    type Error = core::num::TryFromIntError;

    #[inline]
    fn try_from(index: usize) -> Result<Self, Self::Error> {
        u16::try_from(index).map(Self::new)
    }
}

pub use strategy::{EmitStrategy, ManifestStrategyEntry, SubstrateBinding};
pub use r#struct::{FieldSource, LayoutKind, StructField, StructLayout, StructRegistry};
