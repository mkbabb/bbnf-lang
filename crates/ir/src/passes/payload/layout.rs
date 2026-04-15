//! Aggregate payload layout planner.
//!
//! Given a rule's projected `TypeDesc::Tuple(scalar_fields...)`, produce
//! a [`PayloadLayout`] with explicit field offsets respecting natural
//! scalar alignment. Rules whose total layout would exceed
//! [`MAX_PAYLOAD_BYTES`] (16) are skipped — they continue using the
//! existing compound-children pathway.

use std::collections::HashMap;

use crate::types::{GrammarIR, RuleId, TypeDesc};

/// Maximum aggregate payload size in bytes.
///
/// 16 bytes matches the size of a single [`bbnf_tape::TapeRec`] and
/// gives every aggregate at most two 8-byte arena slots. Rules whose
/// scalar tuple exceeds this fall back to the regular compound
/// representation rather than promoting to a heap allocation.
pub const MAX_PAYLOAD_BYTES: u8 = 16;

/// Planned aggregate payload layout for a rule.
///
/// `fields` lists the projected scalar fields with their byte
/// offsets into a 16-byte aggregate buffer. `total_bytes` is the
/// number of bytes the buffer actually occupies — the codegen reads
/// only this many bytes back through
/// [`bbnf_tape::Tape::payload_bytes`].
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PayloadLayout {
    pub fields: Vec<PayloadField>,
    pub total_bytes: u8,
}

/// One aligned scalar field within an aggregate payload.
///
/// The `ty` member is the scalar `TypeDesc` (any variant satisfying
/// [`TypeDesc::is_scalar_payload`]). The `offset` is the byte index
/// into the aggregate buffer where this field's bytes begin —
/// guaranteed to be a multiple of `ty.payload_align_bytes()`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PayloadField {
    pub ty: TypeDesc,
    pub offset: u8,
}

/// Compute aggregate payload layouts for every rule whose `TypeDesc`
/// is a `Tuple` of scalars.
///
/// Returns a map from `RuleId` to the planned layout. Rules whose
/// layout would exceed [`MAX_PAYLOAD_BYTES`] are omitted (they
/// continue to use compound-children storage). Non-Tuple rule types
/// are also omitted — scalar-leaf rules already live on the
/// `PayloadData::InlineScalar` / `PayloadData::WideScalar` paths
/// (AU.6.7); this planner is exclusively for the multi-field
/// aggregate case.
pub fn compute_payload_layouts(ir: &GrammarIR) -> HashMap<RuleId, PayloadLayout> {
    let mut out = HashMap::new();
    for (rule_id, ty) in &ir.types {
        let layout = match ty {
            TypeDesc::Tuple(fields) => {
                // AR.9: KV-pair shape [Span, scalar] — the Span key
                // lives in the record's own (span_lo, span_hi), so
                // only the scalar value needs aggregate storage.
                if is_kv_pair_shape(fields) {
                    plan_layout(&fields[1..])
                } else {
                    plan_layout(fields)
                }
            }
            // AS.2.3: Named struct types — look up the concrete field
            // layout in the struct registry and plan from those fields.
            TypeDesc::Named(sid) => {
                if let Some(fields) = ir.struct_registry.get(sid) {
                    plan_layout(fields)
                } else {
                    continue;
                }
            }
            // Bare scalar rules (e.g. `number -> f64`) are single-field
            // payloads. Span-typed rules use TapeRec.span_lo/span_hi
            // natively and don't need a payload slot.
            td if td.needs_payload_slot() => plan_layout(std::slice::from_ref(td)),
            _ => continue,
        };
        let Some(layout) = layout else {
            continue;
        };
        out.insert(*rule_id, layout);
    }
    out
}

/// Recognize the KV-pair shape: exactly two fields where the first
/// is `TypeDesc::Span` and the second is a scalar payload.
///
/// This is the flattening criterion for `TapeKind::KvPair` — a
/// Seq with this shape can be stored as a single aggregate leaf
/// (key span + value payload) instead of a compound with two
/// children.
pub fn is_kv_pair_shape(fields: &[TypeDesc]) -> bool {
    matches!(fields, [TypeDesc::Span, value] if value.is_scalar_payload())
}

/// Produce a layout plan for a tuple of scalar TypeDescs.
///
/// Walks the fields in declaration order, aligning each field to its
/// natural alignment and bumping the running offset. Returns `None`
/// if any field is non-scalar or the total would exceed
/// [`MAX_PAYLOAD_BYTES`].
pub fn plan_layout(fields: &[TypeDesc]) -> Option<PayloadLayout> {
    let mut offset: u8 = 0;
    let mut planned = Vec::with_capacity(fields.len());
    for f in fields {
        if !f.is_scalar_payload() {
            return None;
        }
        let size = f.payload_size_bytes()?;
        let align = f.payload_align_bytes()?;
        let aligned = (offset + align - 1) & !(align - 1);
        if aligned + size > MAX_PAYLOAD_BYTES {
            return None;
        }
        planned.push(PayloadField {
            ty: f.clone(),
            offset: aligned,
        });
        offset = aligned + size;
    }
    Some(PayloadLayout {
        fields: planned,
        total_bytes: offset,
    })
}
