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
/// is a `Tuple` of scalars, a bare `TypeDesc::Span`, or a bare
/// scalar projected from an Alt body.
///
/// Returns a map from `RuleId` to the planned layout. Rules whose
/// layout would exceed [`MAX_PAYLOAD_BYTES`] are omitted (they
/// continue to use compound-children storage). Non-Tuple, non-Span,
/// non-scalar-Alt rule types are also omitted — regex-bodied scalar
/// rules (e.g. `number = /regex/ -> f64`) live on the
/// `PayloadData::InlineScalar` / `PayloadData::WideScalar` paths
/// (AU.6.7); this planner covers the multi-field aggregate case
/// plus the bare `Span` / scalar-Alt cases.
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
///
/// AV.0.1 (close-out): scalar-Alt rules — Sheets `add_op =
/// "+" -> 0u8 | "-" -> 1u8`, `mul_op`, `unary_prefix`, `compare_op`,
/// `sheet_prefix`, `boolean`, plus any analogous rule whose body
/// is an Alt and whose projected type is a bare scalar
/// (`U8` / `Bool` / `I8` / `U16` / `I16` / `U32` / `I32` / `U64` /
/// `I64` / `F64`) — register a single-field layout `[T @ offset 0,
/// total_bytes = size_of::<T>()]`. The alt composers' per-branch
/// payload-write hoist (see `backend/rust/emitter/alt.rs`) fires
/// on `ctx.payload_layout.is_some()`, so a registered layout is
/// what turns the per-branch writes on; without it each branch
/// emits the literal match without touching the aggregate buffer
/// and the rule epilogue's `PayloadData::Aggregate` commit carries
/// garbage. Gating matches the bare-Span case: leaf-only body
/// (no sub-rule refs), not `TransparentElide`. Single-leaf Map
/// rules (e.g. Sheets `number = /regex/ -> f64`) stay on the
/// `TapeSpanOnly` scalar path because their body is `Map(Regex)`,
/// not `Alt(...)`.
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
            //
            // AV.0.1 close-out: scalar-Alt rules share the
            // eligibility contract — the admission additionally
            // requires the body to be an `Alt`, so Map-bodied
            // scalar rules (`number = /regex/ -> f64`) continue to
            // route through the dedicated `TapeSpanOnly` scalar
            // path (`PayloadData::InlineScalar` / `WideScalar`)
            // rather than the aggregate buffer.
            td if scalar_layout_eligible(ir, *rule_id, td) => {
                plan_layout(std::slice::from_ref(td))
            }
            // Named types have no concrete field layout at the IR
            // layer — typed struct projections are resolved at codegen
            // time via per-backend type tables rather than a centralised
            // registry. Skip; the codegen path handles these directly.
            TypeDesc::Named(_) => continue,
            // Non-Alt-bodied scalar rules (e.g. `number = /regex/
            // -> f64`) live on the scalar
            // `PayloadData::InlineScalar` / `WideScalar` path —
            // one arena slot, no aggregate stack buffer. The Rust
            // emitter dispatches them through
            // `emit_tape_span_only_scalar_*` when the layout is
            // absent. Skipping them here keeps the aggregate
            // planner exclusively for the multi-field tuple case
            // plus the bare-Span and scalar-Alt cases carved out
            // above.
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

/// AV.0.2 / AV.0.1 close-out: gate the single-field scalar
/// aggregate route on type shape, body shape, and materialisation
/// class.
///
/// A rule is eligible when:
/// - its projected type is a bare scalar payload — `TypeDesc::Span`,
///   `Bool`, or any narrow integer / float primitive. Non-scalar
///   types (Tuple, Vec, Option, Enum, Named, …) route through the
///   tuple or structural paths upstream;
/// - the rule body is either an `IrNode::Alt` (scalar-Alt
///   admission — `add_op = "+" -> 0u8 | "-" -> 1u8` and kin) or the
///   projected type is `Span` (the AV.0.2 single-leaf token case).
///   Map-bodied scalar rules (e.g. Sheets `number = /regex/ -> f64`)
///   stay on the dedicated `TapeSpanOnly` scalar path: their single
///   post-match conversion fires via `span_helper_capture` at Map
///   time, with no per-branch hoist to activate;
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
fn scalar_layout_eligible(ir: &GrammarIR, rule_id: RuleId, ty: &TypeDesc) -> bool {
    if !ty.is_scalar_payload() {
        return false;
    }
    let rule = ir.get_rule(rule_id);
    // Span admits single-leaf regex / literal bodies (AV.0.2); the
    // non-Span scalars admit Alt bodies only (AV.0.1 close-out).
    // A Map-bodied scalar rule has a single post-match conversion
    // — `span_helper_capture` / the scalar setter — rather than a
    // per-branch hoist, so it stays off the aggregate path.
    let body_shape_ok = matches!(ty, TypeDesc::Span) || matches!(rule.body, IrNode::Alt(_, _));
    if !body_shape_ok {
        return false;
    }
    if !body_is_leaf_only(&rule.body) {
        return false;
    }
    // AV.0.1 close-out: a scalar-Alt rule whose Ref is consumed by a
    // parent's aggregate-payload layout must stay inlined — otherwise
    // the parent's `__aggregate_buf` loses the field write. CSS L4
    // `length = number, lengthUnit` projects `Tuple([F64, U8])` and
    // takes a `[F64 @ 0, U8 @ 8]` layout; the U8 comes from
    // `lengthUnit`'s Alt branches. If `lengthUnit` becomes `DirectCall`
    // (forced by its own layout via AU.2.5), the Alt branches now
    // write into `lengthUnit`'s own `__aggregate_buf` — `length`'s
    // U8 slot stays zero and the dimension decodes as `px` regardless
    // of the matched unit. Likewise `dirPseudo = ":dir", "(" >>
    // dirKeyword << ")"` projects `Tuple([Span, U8])`, its KV-pair
    // layout consumes the U8 from `dirKeyword`'s Alt branches; a
    // standalone `dirKeyword` layout shifts the u8 into a nested Span
    // record and the `dirPseudo` KvPair reader reads zero. The gate
    // keeps scalar-Alt rules inlined at these compound call sites —
    // `Sheets add_op / mul_op / unary_prefix / compare_op /
    // sheet_prefix / boolean` callers (`add_expr`, `mul_expr`,
    // `unary_expr`, `comparison_expr`, `cell`, `primary`) compose
    // their U8 values into a parent `Tuple` that contains a
    // non-scalar field (`BoxedEnum`, `Vec`, `Option`) so the parent
    // never receives an aggregate layout; admitting those rules is
    // safe. Dimension / KV-pair callers have all-scalar Tuple parents
    // and trigger the veto.
    if ref_breaks_parent_layout(ir, rule_id) {
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

/// AV.0.1 close-out: veto scalar-Alt admission when the rule's Ref
/// feeds a parent rule's aggregate-payload layout. A parent's
/// aggregate layout is populated by its body's per-leaf writes into
/// the shared `__aggregate_buf`; the Alt composer's per-branch
/// payload-write hoist uses the parent's cursor to land the write in
/// the right field. Promoting the child rule to a callable function
/// (AU.2.5 forces `DirectCall` for layout-carrying rules) shifts the
/// write into the child's own record, leaving the parent's aggregate
/// field unwritten. The veto gate keeps the child inlined at any
/// call site whose enclosing rule composes a scalar-tuple or
/// KV-pair layout.
///
/// Returns `true` when admission would break an enclosing aggregate
/// layout — that is, when the candidate rule is referenced by any
/// rule whose projected type is:
///   * a `Tuple(scalar_fields...)` whose `plan_layout` succeeds
///     (bounded by `MAX_PAYLOAD_BYTES` and all fields scalar), or
///   * a KV-pair shape `Tuple([Span, scalar])`.
/// Such a parent's body relies on the candidate's leaves to fill the
/// shared aggregate buffer; forcing the child to carry its own
/// layout orphans the parent's field.
fn ref_breaks_parent_layout(ir: &GrammarIR, rule_id: RuleId) -> bool {
    for parent in &ir.rules {
        if parent.id == rule_id {
            continue;
        }
        let Some(parent_ty) = ir.types.iter().find_map(|(rid, td)| {
            (*rid == parent.id).then_some(td)
        }) else {
            continue;
        };
        let parent_has_layout = match parent_ty {
            TypeDesc::Tuple(fields) => {
                if is_kv_pair_shape(fields) {
                    plan_layout(&fields[1..]).is_some()
                } else {
                    plan_layout(fields).is_some()
                }
            }
            _ => false,
        };
        if !parent_has_layout {
            continue;
        }
        if body_references_rule(&parent.body, rule_id) {
            return true;
        }
    }
    false
}

/// True iff `node` or any descendant is `IrNode::Ref(rule_id)`.
fn body_references_rule(node: &IrNode, rule_id: RuleId) -> bool {
    if let IrNode::Ref(rid) = node {
        return *rid == rule_id;
    }
    let mut found = false;
    node.for_each_child(&mut |child| {
        if !found && body_references_rule(child, rule_id) {
            found = true;
        }
    });
    found
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
