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
    /// A boolean value (produced by `"true" -> true | "false" -> false` constant maps).
    Bool,
    /// Signed 8-bit integer.
    I8,
    /// Unsigned 8-bit integer (e.g., produced by `"px" -> 0u8 | "em" -> 1u8` constant maps).
    U8,
    /// Signed 16-bit integer.
    I16,
    /// Unsigned 16-bit integer.
    U16,
    /// Signed 32-bit integer.
    I32,
    /// Unsigned 32-bit integer (produced by fused hex scan+convert).
    U32,
    /// Signed 64-bit integer.
    I64,
    /// Unsigned 64-bit integer.
    U64,
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

impl TypeDesc {
    /// Resolve a well-known scalar type name to its concrete `TypeDesc`
    /// variant. Returns `None` for unknown names — callers fall back to
    /// `TypeDesc::Named` in that case.
    ///
    /// This is the single source of truth for the
    /// `"f64" → TypeDesc::F64` mapping; both lowering (`lower_map_arrow`)
    /// and the type projection CSP consume it.
    pub fn from_scalar_name(name: &str) -> Option<TypeDesc> {
        match name {
            "f64" => Some(TypeDesc::F64),
            "f32" => Some(TypeDesc::F64), // f32 promoted to f64 (no F32 variant)
            "bool" => Some(TypeDesc::Bool),
            "i8" => Some(TypeDesc::I8),
            "u8" => Some(TypeDesc::U8),
            "i16" => Some(TypeDesc::I16),
            "u16" => Some(TypeDesc::U16),
            "i32" => Some(TypeDesc::I32),
            "u32" => Some(TypeDesc::U32),
            "i64" => Some(TypeDesc::I64),
            "u64" => Some(TypeDesc::U64),
            _ => None,
        }
    }

    /// True if this type is storable inline as a fixed-size scalar payload.
    ///
    /// Scalar payloads are written into the tape's payload buffer via
    /// `push_leaf_with_<T>` and read back via `payload_<T>` — zero
    /// re-parse from the source span.
    ///
    /// `Span` is stored as two packed `u32` values (lo, hi) = 8 bytes
    /// in one payload slot, enabling O(1) sub-span retrieval without
    /// re-scanning the source text.
    pub fn is_scalar_payload(&self) -> bool {
        matches!(
            self,
            TypeDesc::Span
                | TypeDesc::F64
                | TypeDesc::Bool
                | TypeDesc::I8
                | TypeDesc::U8
                | TypeDesc::I16
                | TypeDesc::U16
                | TypeDesc::I32
                | TypeDesc::U32
                | TypeDesc::I64
                | TypeDesc::U64
        )
    }

    /// Width in bytes of a scalar payload; `None` for non-scalar types.
    ///
    /// Used by the tape builder to size the payload slot precisely
    /// instead of always allocating the legacy 8-byte slot.
    pub fn payload_size_bytes(&self) -> Option<u8> {
        match self {
            TypeDesc::Span | TypeDesc::F64 | TypeDesc::I64 | TypeDesc::U64 => Some(8),
            TypeDesc::I32 | TypeDesc::U32 => Some(4),
            TypeDesc::I16 | TypeDesc::U16 => Some(2),
            TypeDesc::Bool | TypeDesc::I8 | TypeDesc::U8 => Some(1),
            _ => None,
        }
    }

    /// Alignment requirement; for primitive scalars this equals the
    /// size in bytes.
    pub fn payload_align_bytes(&self) -> Option<u8> {
        self.payload_size_bytes()
    }

    /// As a Rust source identifier (e.g., for `format_ident!("payload_{}", ...)`).
    /// `None` for non-scalar types.
    pub fn rust_ident(&self) -> Option<&'static str> {
        match self {
            TypeDesc::Span => Some("Span"),
            TypeDesc::F64 => Some("f64"),
            TypeDesc::Bool => Some("bool"),
            TypeDesc::I8 => Some("i8"),
            TypeDesc::U8 => Some("u8"),
            TypeDesc::I16 => Some("i16"),
            TypeDesc::U16 => Some("u16"),
            TypeDesc::I32 => Some("i32"),
            TypeDesc::U32 => Some("u32"),
            TypeDesc::I64 => Some("i64"),
            TypeDesc::U64 => Some("u64"),
            _ => None,
        }
    }
}
