//! Rust-backend per-type-name resolution for the payload layout
//! pass.
//!
//! AW.0.5: implements [`bbnf_ir::passes::NamedTypeResolver`] for the
//! Rust backend. The IR crate owns the trait (pass-side); this
//! module owns the Rust-specific shape table. Per AU.4.2's stated
//! path — "codegen handles struct projections via per-backend type
//! tables, not a central registry" — each backend carries its own
//! sibling resolver module.
//!
//! ## AX.W1r.1 — IR-derived shape table
//!
//! Pre-W1r.1 the resolver held a `static &[NamedTypeBinding]` slice
//! hand-enumerating `"Color"` / `"ColorMix"` with their
//! `(U8, F64, F64, F64, F64)` tuple shape. The table was
//! invariant-violating on two counts: (a) the shape was hand-encoded
//! rather than derived from the grammar's own `-> input : <Name>`
//! projection; (b) the row set was cherry-picked rather than driven
//! off `ir.types`.
//!
//! W1r.1 replaces the static slice with a builder that walks
//! `ir.types` once at `from_ir` time. For every
//! `(rule_id, TypeDesc::Named(sid))` entry we inspect the declaring
//! rule's body, kernel-walk through structural wrappers
//! ([`IrNode::Map`] / [`IrNode::OptionalWhitespace`] /
//! [`IrNode::Skip`] / [`IrNode::Next`]) to the structural core, and
//! infer the scalar tuple shape from that kernel:
//!
//! - **Seq kernel** (e.g. a CSS L4 `colorFunction` body) — collect
//!   each child's projected scalar [`TypeDesc`] from `ir.type_map`,
//!   falling back to the child's declared rule type when the node
//!   map is unpopulated for that node (conservative).
//!   `Option<scalar>` children unwrap to the scalar itself so the
//!   field layout matches the NaN-absent / 0xFF-absent convention
//!   the emitter applies.
//! - **Leaf kernel** (Regex / Literal / a standalone Map over a
//!   regex) — the projected value is an arena handle
//!   `(U32 offset, U32 length)`. Mirrors the
//!   [`compute_payload_layouts_with_resolver`] universal fallback
//!   at the Named admission arm.
//! - **Everything else** — no shape; the resolver declines and the
//!   layout pass's universal fallback (consulted after the resolver)
//!   carries the owned-string / byte-vec names. Keeps the backend
//!   resolver clean of speculative shapes.
//!
//! The resulting `FxHashMap<StringId, Vec<TypeDesc>>` is the
//! resolver's entire state. No static table survives; no
//! per-grammar branches.
//!
//! [`compute_payload_layouts_with_resolver`]: bbnf_ir::passes::compute_payload_layouts_with_resolver

use rustc_hash::FxHashMap;

use bbnf_ir::passes::NamedTypeResolver;
use bbnf_ir::{GrammarIR, IrNode, RuleId, StringId, TypeDesc};

use super::peel::unwrap_structural_wrappers;

/// Rust-backend resolver for backend-specific named types.
///
/// Holds a precomputed map from interned name [`StringId`] to the
/// scalar tuple shape the layout pass admits for that name.
/// Population happens once at [`Self::from_ir`] time from
/// `ir.types` + `ir.type_map`; the resolver trait impl does a
/// single hash-map lookup per call.
pub struct RustNamedTypes<'ir> {
    strings: &'ir [String],
    bindings: FxHashMap<StringId, Vec<TypeDesc>>,
}

impl<'ir> RustNamedTypes<'ir> {
    /// Build the resolver by walking `ir.types` for every
    /// `TypeDesc::Named(sid)` entry and inferring the scalar tuple
    /// shape from the declaring rule's body.
    ///
    /// O(rules + named_types); cheap enough to build once per
    /// compile. See module docs for the inference strategy.
    pub fn from_ir(ir: &'ir GrammarIR) -> Self {
        let mut bindings: FxHashMap<StringId, Vec<TypeDesc>> = FxHashMap::default();
        for (rule_id, td) in &ir.types {
            let TypeDesc::Named(sid) = td else { continue };
            if bindings.contains_key(sid) {
                // Multiple rules may project the same `Named(sid)`
                // (e.g. CSS L4 `colorFunction` / `colorFn` /
                // `colorMix` all carry `-> input : Color`). First
                // rule wins — all declaring rules share the same
                // shape by grammar invariant.
                continue;
            }
            if let Some(shape) = infer_named_shape(ir, *rule_id) {
                bindings.insert(*sid, shape);
            }
        }
        Self { strings: &ir.strings, bindings }
    }

    /// Borrow the underlying string table.
    #[inline]
    pub fn strings(&self) -> &'ir [String] {
        self.strings
    }

    /// Number of grammar-declared named types the resolver admits.
    /// Useful for diagnostics + tests.
    #[inline]
    pub fn binding_count(&self) -> usize {
        self.bindings.len()
    }
}

impl<'ir> NamedTypeResolver for RustNamedTypes<'ir> {
    #[inline]
    fn resolve_named(&self, sid: StringId) -> Option<TypeDesc> {
        self.bindings
            .get(&sid)
            .map(|fields| TypeDesc::Tuple(fields.clone()))
    }
}

/// Infer the scalar tuple shape for a rule whose projected
/// [`TypeDesc`] is `Named(sid)`.
///
/// Returns `Some(fields)` when the rule body's kernel yields a
/// well-formed scalar tuple; `None` when inference declines (the
/// layout pass's universal fallback then carries names like
/// `"String"` / `"Bytes"`). Never panics.
fn infer_named_shape(ir: &GrammarIR, rule_id: RuleId) -> Option<Vec<TypeDesc>> {
    let rule = ir.get_rule(rule_id);
    let kernel = unwrap_structural_wrappers(&rule.body);

    match kernel {
        IrNode::Seq(children) => {
            // Collect each child's scalar projection. A child Ref
            // resolves through `ir.types`; other nodes consult the
            // TypeMap. Unwrap `Option<scalar>` so the layout matches
            // the NaN-absent convention.
            let mut fields = Vec::with_capacity(children.len());
            for child in children {
                let child_kernel = unwrap_structural_wrappers(child);
                let ty = child_scalar_type(ir, child_kernel)?;
                fields.push(ty);
            }
            // A bare `Span` tuple element would survive
            // `is_scalar_payload` but the KV-pair shape arm in the
            // layout pass already handles the `[Span, scalar]`
            // case. Still admit — the layout planner re-checks via
            // `plan_layout`.
            Some(fields)
        }
        // Single-leaf kernel (e.g. JSON `string = /regex/ ->
        // decode_json_string_to_arena(input) : String`). The
        // grammar is projecting a borrow-via-frame value: the
        // runtime emits via `push_leaf_borrowed_string`
        // (child_off=NONE) on the fast path and
        // `push_leaf_with_arena_frame` (4-byte length prefix +
        // bytes) on the escape-decode path. Neither path writes
        // an `(offset, length)` aggregate to the payload column,
        // so synthesising `[U32, U32]` produces a layout the
        // runtime never populates — the materialiser's
        // `payload_bytes(rec, 8)` returns `None` on every borrow
        // and reads stale length-prefix bytes on every escape
        // path.
        //
        // B5.W0.2 (Cluster B): admit single-leaf admissions as
        // `[Span]` instead. The materialiser's Span-field arm
        // already routes through the frame's own `(span_lo,
        // span_hi)` slots — no aggregate buffer round-trip; one
        // pluggable read path shared with bare-`Span` admissions
        // (BBNF identifier / literal / comment, Sheets
        // identifiers, …). The frame layout is the runtime fact;
        // the resolver projection now matches it.
        IrNode::Regex(_) | IrNode::Literal(_) => {
            Some(vec![TypeDesc::Span])
        }
        // Alt / Repeat / Ref / Epsilon / TokenDispatch / unwrapped
        // Map / OptionalWhitespace / Skip / Next kernels are not
        // structurally scalar — decline. The layout pass's
        // universal fallback picks up `"String"` / `"Bytes"` names
        // below this.
        _ => None,
    }
}

/// Project a child node of a Seq body to its scalar-tuple field
/// type. Follows `Ref`s through `ir.types`; consults `ir.type_map`
/// for other nodes. Unwraps `Option<scalar>` to the scalar itself.
/// Returns `None` when the child does not admit a scalar projection
/// (then the parent's shape inference declines, and the layout pass
/// falls through to its universal fallback).
fn child_scalar_type(ir: &GrammarIR, child: &IrNode) -> Option<TypeDesc> {
    let ty = match child {
        IrNode::Ref(target) => ir
            .types
            .iter()
            .find(|(id, _)| *id == *target)
            .map(|(_, t)| t.clone())?,
        IrNode::Repeat { inner, lo: 0, hi: 1 } => {
            // Collapsed `(scalar)?` — recurse into the inner and
            // keep the scalar. The layout pass's NaN-absent / tag=
            // sentinel convention covers the absence.
            let inner_kernel = unwrap_structural_wrappers(inner);
            return child_scalar_type(ir, inner_kernel);
        }
        _ => {
            let dag = ir.dag.as_ref()?;
            let nid = dag.node_for(child)?;
            let map = ir.type_map.as_ref()?;
            map.node_type(nid).cloned()?
        }
    };
    Some(unwrap_option_scalar(ty))
}

/// Unwrap `TypeDesc::Option(inner)` when the inner is itself a
/// scalar payload. Leaves non-Option types untouched.
fn unwrap_option_scalar(ty: TypeDesc) -> TypeDesc {
    match ty {
        TypeDesc::Option(inner) if inner.is_scalar_payload() => *inner,
        other => other,
    }
}
