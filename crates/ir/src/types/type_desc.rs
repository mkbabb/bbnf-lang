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
    /// A heterogeneous-alternation join obligation produced by the type
    /// projection CSP when an `Alt`'s branch types disagree on payload
    /// shape.
    ///
    /// AZ-III.W3a.3 — replaces the historical silent `BoxedEnum`
    /// fallback at `passes::types::constraint::revise::join_types`. The
    /// obligation carries the structurally distinct branch types so
    /// downstream consumers can both (a) emit the same `Box<Enum>`
    /// layout that `BoxedEnum` expanded to, and (b) surface the
    /// alternation's branch types as a named diagnostic via
    /// `GrammarIR::type_obligations`.
    ///
    /// Invariant: the embedded branch list is the deduplicated set of
    /// distinct branch types observed at join time, ordered by first
    /// appearance — never empty, never a single homogeneous element.
    HeterogeneousAltJoin(Vec<TypeDesc>),
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
            // `Span` admits the grammar-level `-> Span` shorthand.
            // Backend-agnostic: Rust resolves to `parse_that::Span<'_>`;
            // other backends map to their native span representation.
            "Span" => Some(TypeDesc::Span),
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
    /// Includes `Span` (packed as two `u32` values = 8 bytes) and all
    /// numeric/bool primitives. Used by aggregate layout planning,
    /// KvPair detection, and field-level payload packing.
    ///
    /// For rule-level routing (deciding whether a standalone rule uses
    /// the payload-bearing codegen path), use [`Self::needs_payload_slot`]
    /// instead — Span-typed rules already carry their span in `TapeRec`
    /// and don't need a payload slot.
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

    /// True if this type contains a scalar payload at any nesting depth —
    /// directly, inside a `Tuple`, or behind an `Option` / `Vec`.
    ///
    /// The lowering pipeline wraps typed-leaf markers in `Tuple([Span,
    /// payload])` for keyword-discriminator alternatives where the
    /// branch carries a captured source span alongside its semantic
    /// payload (e.g. `"row" -> 0u8` lowers through `Skip(spanned)` to
    /// `Tuple([Span, U8])`). The audit pass's coverage policy treats a
    /// marker as `Mapped` whenever the layout's projected type reaches
    /// a scalar at any depth — flat and Tuple-wrapped scalars are both
    /// admitted under the single layout-coverage decision surface
    /// (`feedback_pluggable-components`).
    pub fn has_scalar_payload(&self) -> bool {
        if self.is_scalar_payload() {
            return true;
        }
        match self {
            TypeDesc::Tuple(parts) => parts.iter().any(|t| t.has_scalar_payload()),
            TypeDesc::Option(inner) | TypeDesc::Vec(inner) => inner.has_scalar_payload(),
            _ => false,
        }
    }

    /// True if a standalone rule of this type requires a payload slot.
    ///
    /// `Span`-typed rules store their span natively in `TapeRec.span_lo/
    /// span_hi` — no payload buffer entry needed. All other scalar types
    /// (f64, bool, u8, etc.) must go through `push_leaf_with_<T>`.
    ///
    /// Used by the emitter's rule-function routing to decide between the
    /// `TapeSpanOnly` path (normal span construction) and the
    /// `TapeSpanOnlyScalar` path (payload-bearing leaf).
    pub fn needs_payload_slot(&self) -> bool {
        self.is_scalar_payload() && !matches!(self, TypeDesc::Span)
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

    /// True for `BoxedEnum` and the heterogeneous-alt join obligation.
    ///
    /// AZ-III.W3a.3: `HeterogeneousAltJoin` is the named-obligation
    /// successor to the historical silent `BoxedEnum` fallback at
    /// `passes::types::constraint::revise::join_types`. Every codegen
    /// site that previously branched on `BoxedEnum` consults this
    /// helper instead so the obligation lowers to the same
    /// `Box<Enum>` layout while remaining structurally distinct in
    /// `ir.types`.
    pub fn is_boxed_enum_layout(&self) -> bool {
        matches!(
            self,
            TypeDesc::BoxedEnum | TypeDesc::HeterogeneousAltJoin(_)
        )
    }
}
