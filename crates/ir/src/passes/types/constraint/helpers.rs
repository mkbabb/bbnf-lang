//! Internal solver helpers shared by every constraint impl: assign once,
//! project a Seq's child types into a result type, and join (LUB) Alt
//! branch types.

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
pub(super) fn project_seq_type(child_types: &[TypeDesc], preserve_spans: bool) -> TypeDesc {
    if child_types.is_empty() {
        return TypeDesc::Tuple(vec![]);
    }
    if child_types.len() == 1 {
        return child_types[0].clone();
    }

    // Span compression: collapse consecutive Spans (unless preserved).
    let effective: Vec<&TypeDesc> = if preserve_spans {
        child_types.iter().collect()
    } else {
        let mut result: Vec<&TypeDesc> = Vec::new();
        for ty in child_types {
            if ty == &TypeDesc::Span {
                if result.last().map_or(true, |last| *last != &TypeDesc::Span) {
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
pub(super) fn join_types(branch_types: &[TypeDesc]) -> TypeDesc {
    if branch_types.is_empty() {
        return TypeDesc::Tuple(vec![]);
    }
    let first = &branch_types[0];
    if branch_types.iter().all(|t| t == first) {
        first.clone()
    } else {
        TypeDesc::BoxedEnum
    }
}
