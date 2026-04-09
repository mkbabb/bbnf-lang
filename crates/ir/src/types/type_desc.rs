//! `TypeDesc` — backend-agnostic serialized type description.
//!
//! Captures the essential structure of the type a rule produces without
//! depending on `syn` or any specific type system representation. Each
//! backend resolves these to its own concrete type names.

use serde::{Deserialize, Serialize};

use super::StringId;

/// Serialized type information for a rule's output.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, Hash)]
pub enum TypeDesc {
    /// A borrowed string span: `Span<'a>` in Rust, `string` in TS.
    Span,
    /// A 64-bit float (produced by fused number scan+convert).
    F64,
    /// An unsigned 32-bit integer (produced by fused hex scan+convert).
    U32,
    /// An optional value.
    Option(Box<TypeDesc>),
    /// A vector of values.
    Vec(Box<TypeDesc>),
    /// A fixed-size tuple.
    Tuple(Vec<TypeDesc>),
    /// A boxed enum variant: `Box<EnumName<'a>>`.
    BoxedEnum,
    /// An enum variant: `EnumName<'a>`.
    Enum,
    /// A named type (for custom mapping results).
    Named(StringId),
}
