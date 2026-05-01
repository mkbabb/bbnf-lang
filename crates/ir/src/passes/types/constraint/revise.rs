//! Constraint-revise primitives — domain assignment, Seq-child
//! projection, Alt-branch type-join (LUB).
//!
//! Shared by every constraint impl in this module: AltConstraint,
//! SeqConstraint, GroundConstraint, ReferenceConstraint, the operator
//! constraints. The contract is "single-step revise": each helper
//! mutates one variable's domain or projects across a fixed-shape
//! child set; no propagation orchestration lives here.
//!
//! Tranche Y.10: the `project_seq_type` and `join_types` helpers take
//! `&[&TypeDesc]` instead of `&[TypeDesc]` so that constraint
//! `revise` methods can extract child types via `.solved.as_ref()`
//! instead of cloning each child up front. The old contract forced
//! 2N TypeDesc clones per Seq revise and N+1 clones per Alt revise;
//! the new contract trims those to N and 1 respectively.

use csp_solver::variable::Variable;

use crate::TypeDesc;
use crate::passes::types::obligation::dedup_branch_types;
use crate::passes::types::type_map::try_flatten_pair;

use super::TypeVarId;
use super::domain::TypeDomain;

/// Assign a type to a variable's domain. Returns true if the type changed.
pub(super) fn assign(vars: &mut [Variable<TypeDomain>], var: TypeVarId, ty: TypeDesc) -> bool {
    let slot = &mut vars[var as usize].domain.solved;
    if slot.as_ref() == Some(&ty) {
        false
    } else {
        *slot = Some(ty);
        true
    }
}

/// Compute the type of a Seq node from its children's types.
///
/// Applies Span compression: consecutive Span children collapse to a single Span.
/// Applies try_flatten_pair: (T, Vec<T>) -> Vec<T>.
///
/// Takes a slice of references so callers can borrow from the CSP
/// variable array rather than clone each child up front (Tranche Y.10).
/// The final owned result still requires cloning the specific elements
/// that become part of `TypeDesc::Tuple` or the recursion target.
pub(super) fn project_seq_type(child_types: &[&TypeDesc], preserve_spans: bool) -> TypeDesc {
    if child_types.is_empty() {
        return TypeDesc::Tuple(vec![]);
    }
    if child_types.len() == 1 {
        return child_types[0].clone();
    }

    // Span compression: collapse consecutive Spans (unless preserved).
    let effective: Vec<&TypeDesc> = if preserve_spans {
        child_types.to_vec()
    } else {
        let mut result: Vec<&TypeDesc> = Vec::with_capacity(child_types.len());
        for &ty in child_types {
            if ty == &TypeDesc::Span {
                if result.last().is_none_or(|last| *last != &TypeDesc::Span) {
                    result.push(ty);
                }
            } else {
                result.push(ty);
            }
        }
        result
    };

    match effective.len() {
        0 => TypeDesc::Span,
        1 => effective[0].clone(),
        2 => {
            if let Some(flat) = try_flatten_pair(effective[0], effective[1]) {
                flat
            } else {
                TypeDesc::Tuple(effective.into_iter().cloned().collect())
            }
        }
        _ => TypeDesc::Tuple(effective.into_iter().cloned().collect()),
    }
}

/// Compute the join (least upper bound) of alternation branch types.
///
/// Tranche Y.10: takes `&[&TypeDesc]` so the caller can borrow from
/// variable slots without cloning N child types per revise call. The
/// homogeneous case still clones the one representative branch into
/// the return; the heterogeneous case lifts the distinct branch
/// types into a `TypeDesc::HeterogeneousAltJoin` named-obligation
/// successor to the historical silent `BoxedEnum` fallback.
///
/// AU.2.5: a branch shaped `Tuple([Span, T])` where `T` is a scalar
/// payload carries the same runtime value as a bare `T` — the Span
/// came from `factor_common_prefixes` splitting a shared literal byte
/// out of the rule's branches (`"px" | "cm" | "mm"` becomes
/// `Seq(Literal("p"), Alt("x" | "c" | "t"))`), so the prefix Span is
/// structural noise, not semantic payload. Extend the homogeneity
/// check to accept these branches: if every branch's "effective" type
/// agrees (a bare scalar or a `Tuple([Span, scalar])` of the same
/// scalar), the join is that scalar. This restores correct types for
/// `absoluteLengthUnit`, `relativeLengthUnit`, and every other Alt
/// whose branches were broken up by the trie-style byte factoring.
///
/// AZ-III.W3a.3: when none of the homogeneity reductions fire the
/// join no longer collapses silently to `BoxedEnum`. Instead the
/// distinct branch types are lifted into
/// `TypeDesc::HeterogeneousAltJoin(branches)` so the obligation is
/// surfaced explicitly in `ir.types`, the post-projection
/// `GrammarIR::type_obligations` stream, and any downstream
/// diagnostic consumer. Per AZ-III invariant 7 (no silent fallback)
/// and W3a hard gate 4 (no live `BoxedEnum` literal under the
/// constraint solver), the join site no longer hides a
/// heterogeneous alternation behind the boxed-enum codegen layout.
pub(super) fn join_types(branch_types: &[&TypeDesc]) -> TypeDesc {
    if branch_types.is_empty() {
        return TypeDesc::Tuple(vec![]);
    }
    let first = branch_types[0];
    if branch_types.iter().all(|t| *t == first) {
        return first.clone();
    }

    // AU.2.5: cross-shape homogeneity for factored-prefix branches.
    let first_effective = effective_payload_type(first);
    if let Some(common) = first_effective {
        if branch_types
            .iter()
            .all(|t| effective_payload_type(t).as_ref() == Some(&common))
        {
            return common;
        }
    }

    // AZ-III.W3a.3 — named obligation in place of the silent
    // boxed-enum fallback. The deduplicated branch list canonicalises
    // the obligation payload so structurally equivalent Alts produce
    // identical `TypeDesc::HeterogeneousAltJoin` values regardless of
    // branch repetition.
    TypeDesc::HeterogeneousAltJoin(dedup_branch_types(branch_types))
}

/// Extract the semantically meaningful payload type from a branch's
/// projected `TypeDesc`, stripping the structural `Span` prefixes
/// inserted by `factor_common_prefixes`.
///
/// `factor_common_prefixes` re-groups alternations by common literal
/// prefixes recursively. A branch like `"aliceblue" -> 0xF0F8FFFFu32`
/// may end up wrapped as `Tuple([Span, Tuple([Span, Tuple([Span,
/// U32])])])` once multi-level factoring runs (e.g. the CSS L4
/// `namedColor` alt with 148 branches produces a multi-level trie).
/// Each prefix `Span` originated from a shared literal byte split out
/// by the trie; only the innermost type carries semantic payload.
///
/// Accepts:
/// - a bare non-Span scalar payload (`needs_payload_slot`); returns
///   it.
/// - `Tuple([Span, T])` where `T` recursively resolves to an effective
///   payload type; returns the innermost type.
///
/// Returns `None` for anything else — bare `Span` (which is handled
/// by the outer homogeneity check), multi-field tuples without a
/// leading Span prefix, BoxedEnum, Vec, Option, Named, Enum. Those
/// shapes stay compound under the Alt join.
fn effective_payload_type(ty: &TypeDesc) -> Option<TypeDesc> {
    if ty.needs_payload_slot() {
        return Some(ty.clone());
    }
    if let TypeDesc::Tuple(elems) = ty {
        if elems.len() == 2 && elems[0] == TypeDesc::Span {
            return effective_payload_type(&elems[1]);
        }
    }
    None
}
