//! Aggregate payload layout planning.
//!
//! For each rule whose `TypeDesc` is `Tuple(scalar_fields...)` where every
//! field passes [`TypeDesc::is_scalar_payload`], compute field offsets
//! respecting alignment and total bytes. Layouts that exceed
//! [`layout::MAX_PAYLOAD_BYTES`] (16) are not eligible — the rule remains a
//! tape-compound.
//!
//! Consumers: codegen (rule prelude/epilogue, view layer accessors). The
//! rule body emits scalar writes into a stack-allocated 16-byte buffer at
//! the field offsets recorded here, then a single fused-builder
//! `push_leaf_with` call carrying an `Aggregate` payload at the epilogue
//! commits the bytes to the runtime arena. The view reads back the
//! `total_bytes` payload slice through the runtime accessor.

mod layout;
pub mod named_types;
mod scalar_routing;

pub use layout::{
    compute_payload_layouts, compute_payload_layouts_with_resolver, is_kv_pair_shape, plan_layout,
    plan_layout_with_cap, PayloadField, PayloadLayout, LARGE_PAYLOAD_MAX, MAX_PAYLOAD_BYTES,
};
pub use named_types::{NamedTypeResolver, NullResolver};
pub use scalar_routing::scalar_range_includes_sentinel;
