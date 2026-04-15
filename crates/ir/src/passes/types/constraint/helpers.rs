//! Internal solver helpers shared by every constraint impl: assign once,
//! project a Seq's child types into a result type, and join (LUB) Alt
//! branch types.
//!
//! Tranche Y.10: the `project_seq_type` and `join_types` helpers take
//! `&[&TypeDesc]` instead of `&[TypeDesc]` so that constraint
//! `revise` methods can extract child types via `.solved.as_ref()`
//! instead of cloning each child up front. The old contract forced
//! 2N TypeDesc clones per Seq revise and N+1 clones per Alt revise;
//! the new contract trims those to N and 1 respectively.

use csp_solver::variable::Variable;

use crate::TypeDesc;
use crate::passes::types::utils::try_flatten_pair;

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
/// the return; the heterogeneous case returns `BoxedEnum` with zero
/// clones.
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

    TypeDesc::BoxedEnum
}

/// Extract the semantically meaningful payload type from a branch's
/// projected `TypeDesc`, stripping the structural `Span` inserted by
/// `factor_common_prefixes`.
///
/// Accepts:
/// - a bare scalar payload (`needs_payload_slot()`); returns it
/// - `Tuple([Span, T])` where `T` is a scalar payload; returns `T`
///
/// Returns `None` for anything else — compound types stay compound
/// under the Alt join.
fn effective_payload_type(ty: &TypeDesc) -> Option<TypeDesc> {
    if ty.needs_payload_slot() {
        return Some(ty.clone());
    }
    if let TypeDesc::Tuple(elems) = ty {
        if elems.len() == 2 && elems[0] == TypeDesc::Span && elems[1].needs_payload_slot() {
            return Some(elems[1].clone());
        }
    }
    None
}
