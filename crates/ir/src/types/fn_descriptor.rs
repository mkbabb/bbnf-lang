//! `FnDescriptor` — host function descriptor for `IrNode::Map`.
//!
//! Most variants are produced by the compiler during lowering and
//! specialization (`EnumWrap`, `BoxWrap`, `NumberConvert`, `HexConvert`,
//! `SpanCapture`). The user-facing variant is `Expr`, which carries a
//! `MapExpr` tree plus an optional return type that overrides type projection.

use serde::{Deserialize, Serialize};

use super::{MapExpr, StringId, TypeDesc};

/// Describes a host-side function referenced by `IrNode::Map`.
///
/// Internal variants (EnumWrap, BoxWrap, SpanCapture, NumberConvert, HexConvert)
/// are produced by the compiler during lowering and specialization.
/// `Expr` is the only user-facing variant — all `->` map syntax produces it.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub enum FnDescriptor {
    /// Wrap the parse result in an enum variant: `EnumName::VariantName(result)`.
    EnumWrap {
        /// The variant name (e.g., `"Value"` for `JsonEnum::Value`).
        variant: StringId,
    },

    /// Box the parse result: `Box::new(result)`.
    BoxWrap,

    /// Fused numeric regex scan + f64 conversion.
    ///
    /// Internal specialization of `Expr { Input, return_type: F64 }` when
    /// the inner IR node is a numeric regex. The `allow_leading_dot`
    /// flag is derived at lowering time from
    /// `RegexClass::Numeric { allow_leading_dot, .. }`:
    ///
    /// - `false` → JSON-strict digits (no bare `.5`). Codegen emits
    ///   `scan_number_strict_f64(state)` → `Option<f64>`.
    /// - `true` → CSS-permissive digits (bare `.5` admitted). Codegen
    ///   emits `scan_number_f64(state)` → `Option<f64>`, routed through
    ///   `GENERIC_NUMBER_CONFIG`.
    NumberConvert { allow_leading_dot: bool },

    /// Fused hex regex scan + user conversion function.
    /// Internal specialization of `Expr { FnCall(name, [Input]), return_type: U32 }` when
    /// inner is a hex digit regex. Codegen emits inline char-class loop + conversion call.
    HexConvert { fn_path: StringId },

    /// Span capture: parse inner expression for validation, discard result, return Span.
    /// Used by `@{expr}` syntax to capture raw text while validating structure.
    SpanCapture,

    /// User-facing value expression. All `->` map syntax lowers to this variant.
    /// The `MapExpr` tree is fully transparent to IR passes for constant folding,
    /// type projection, and pattern-based optimization.
    Expr {
        expr: MapExpr,
        return_type: Option<TypeDesc>,
    },
}
