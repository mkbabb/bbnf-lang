//! Shared helpers used across the Rust schema emitter sub-modules.

use bbnf_ir::TypeDesc;

/// Whether a `TypeDesc` recursively contains any enum-typed child.
/// Used to skip variants that carry only leaf data (Span/F64/Named).
pub(super) fn type_has_enum_children(td: &TypeDesc) -> bool {
    match td {
        TypeDesc::BoxedEnum | TypeDesc::Enum => true,
        TypeDesc::Span | TypeDesc::F64 | TypeDesc::U32 => false,
        TypeDesc::Option(inner) => type_has_enum_children(inner),
        TypeDesc::Vec(inner) => type_has_enum_children(inner),
        TypeDesc::Tuple(elems) => elems.iter().any(type_has_enum_children),
        TypeDesc::Named(_) => false,
    }
}
