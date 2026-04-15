//! Aggregate payload layout planner.
//!
//! Given a rule's projected `TypeDesc::Tuple(scalar_fields...)`, produce
//! a [`PayloadLayout`] with explicit field offsets respecting natural
//! scalar alignment. Rules whose total layout would exceed
//! [`MAX_PAYLOAD_BYTES`] (16) are skipped — they continue using the
//! existing compound-children pathway.

use std::collections::HashMap;

use crate::passes::materialization::MaterializationClass;
use crate::types::{GrammarIR, IrNode, RuleId, TypeDesc};

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
/// is a `Tuple` of scalars or a bare `TypeDesc::Span`.
///
/// Returns a map from `RuleId` to the planned layout. Rules whose
/// layout would exceed [`MAX_PAYLOAD_BYTES`] are omitted (they
/// continue to use compound-children storage). Non-Tuple, non-Span
/// rule types are also omitted — scalar-leaf rules already live on
/// the `PayloadData::InlineScalar` / `PayloadData::WideScalar` paths
/// (AU.6.7); this planner covers the multi-field aggregate case
/// plus the bare `Span` case.
///
/// AV.0.2: bare-`Span` rules — BBNF's `identifier`, `literal`,
/// `regex`, `big_comment`, `comment`, `string_lit`, and Sheets'
/// `-> input : Span` rules — pack the matched `(u32 lo, u32 hi)`
/// pair into a one-field aggregate. Span is `is_scalar_payload`
/// (8 bytes, 4-aligned), so `plan_layout` admits it directly. The
/// resulting layout has a single `[Span @ offset 0, total_bytes 8]`
/// field that the rule body's leaf emitter writes after the regex /
/// literal match succeeds, and the existing aggregate epilogue
/// commits via `push_leaf_with(PayloadData::Aggregate(..))`. No
/// widening of `MAX_PAYLOAD_BYTES`; no new emitter path.
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
            // AV.0.2: standalone `Span` projects into the same
            // aggregate route as KV-pairs and scalar tuples. The
            // single field carries (lo, hi) packed into 8 bytes; the
            // emitter's leaf path writes it post-match, the epilogue
            // commits via `PayloadData::Aggregate(&buf[..8])`. Gated
            // on body shape + materialisation class so only token-
            // shaped rules — bodies with no `IrNode::Ref(_)` whose
            // own materialisation already places them on the
            // callable side of the codegen split — opt in. Rules
            // whose body is a sub-rule reference (e.g. BBNF
            // `pretty_hint = identifier , (...)?`) keep the compound
            // pathway because the Span value lives in the children's
            // own typed leaves, not the rule's own span; rules whose
            // materialisation class is `TransparentElide` (e.g. JSON
            // `comma = "," ?w`) are inlined at every call site and
            // have no own emission point to commit a layout against.
            TypeDesc::Span if span_layout_eligible(ir, *rule_id) => {
                plan_layout(std::slice::from_ref(ty))
            }
            // Named types have no concrete field layout at the IR
            // layer — typed struct projections are resolved at codegen
            // time via per-backend type tables rather than a centralised
            // registry. Skip; the codegen path handles these directly.
            TypeDesc::Named(_) => continue,
            // Bare scalar rules (e.g. `number -> f64`) live on the
            // scalar `PayloadData::InlineScalar` / `WideScalar` path
            // — one arena slot, no aggregate stack buffer. The Rust
            // emitter dispatches them through
            // `emit_tape_span_only_scalar_*` when the layout is
            // absent. Skipping them here keeps the aggregate planner
            // exclusively for the multi-field tuple case plus the
            // bare-Span case carved out above.
            td if td.needs_payload_slot() => continue,
            _ => continue,
        };
        let Some(layout) = layout else {
            continue;
        };
        out.insert(*rule_id, layout);
    }
    out
}

/// AV.0.2: gate the bare-`Span` aggregate route on body shape +
/// materialisation class.
///
/// A rule is eligible when:
/// - its body is a *leaf-only* expression — no `IrNode::Ref(_)`
///   anywhere in the tree, so the rule's own span fully determines
///   the typed payload (no sub-rule contributes its own span);
/// - its head materialisation class is non-`TransparentElide` — the
///   rule has its own callable function in the emitted backend; a
///   `TransparentElide` rule is inlined at every call site and has
///   no rule push to attach a payload to.
///
/// Without these gates the layout pass would over-promote: BBNF
/// `pretty_hint = identifier, (...)` and `import_directive` collapse
/// to a `Span` projected type via Optional/Span compression but
/// their bodies push children through sub-rule calls, so the rule's
/// own push must remain a `push_compound`. JSON `comma = "," ?w` is
/// leaf-only at the structural level but inlines into every caller
/// — there is no `__comma` function to host the aggregate epilogue,
/// so adding a layout would force the inline analyser to demote it
/// to a `DirectCall` and inflate the tape with redundant compound
/// records that no longer round-trip the existing goldens.
fn span_layout_eligible(ir: &GrammarIR, rule_id: RuleId) -> bool {
    let rule = ir.get_rule(rule_id);
    if !body_is_leaf_only(&rule.body) {
        return false;
    }
    // AV.0.2 (close-out): permissive gate on materialisation. The
    // CSP-refined materialisation map (`ir.materialization`) is only
    // populated for non-`structural` pipeline runs — BBNF's own
    // bootstrap compiles with `structural = true` so `preserve_identity`
    // is stamped on every rule and the CSP pipeline is skipped. A
    // strict `Some(class) != TransparentElide` gate silently excludes
    // every structural-build Span rule (BBNF `identifier`, `literal`,
    // `regex`, `big_comment`, `comment`) from the layout pipeline. The
    // permissive reading is sound: `TransparentElide` only exists as a
    // refined materialisation decision; when the map is unpopulated no
    // rule has been downgraded to transparent inlining. Admit the
    // layout whenever the gate does not have explicit `TransparentElide`
    // evidence against the rule.
    !matches!(
        rule_head_materialization(ir, rule_id),
        Some(MaterializationClass::TransparentElide),
    )
}

/// True iff `node` and every descendant carry no `IrNode::Ref(_)`.
///
/// A leaf-only body's runtime side effect is bounded by literal /
/// regex / `OptionalWhitespace` /
/// `Map`-of-leaf scanner calls — every byte the body consumes lives
/// in the rule's own `(span_lo, state.offset)` window, so the rule's
/// projected Span semantically equals the rule's whole-rule span and
/// `PayloadData::Aggregate(&buf[..8])` is the right commitment.
fn body_is_leaf_only(node: &IrNode) -> bool {
    if matches!(node, IrNode::Ref(_)) {
        return false;
    }
    let mut ok = true;
    node.for_each_child(&mut |child| {
        if ok && !body_is_leaf_only(child) {
            ok = false;
        }
    });
    ok
}

/// Resolve the materialisation class for a rule's head node via
/// `ir.dag` + `ir.materialization`.
///
/// Returns `None` when the dag or the materialisation map has not
/// been populated yet — callers treat that as a conservative
/// "unknown" and skip layout admission rather than guessing.
fn rule_head_materialization(ir: &GrammarIR, rule_id: RuleId) -> Option<MaterializationClass> {
    let rule = ir.get_rule(rule_id);
    let dag = ir.dag.as_ref()?;
    let node_id = dag.node_for(&rule.body)?;
    ir.materialization.get(&node_id).copied()
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
