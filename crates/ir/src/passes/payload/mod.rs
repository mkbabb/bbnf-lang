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
//! the field offsets recorded here, then a single
//! [`bbnf_tape::TapeBuilder::push_leaf_with_aggregate`] call at the
//! epilogue commits the bytes to the tape's payload buffer. The view
//! reads back via [`bbnf_tape::Tape::payload_bytes`] using `total_bytes`.

mod layout;

pub use layout::{
    compute_payload_layouts, is_kv_pair_shape, plan_layout, PayloadField, PayloadLayout,
    MAX_PAYLOAD_BYTES,
};
