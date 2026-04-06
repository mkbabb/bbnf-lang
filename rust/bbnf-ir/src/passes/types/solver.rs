//! CSP solver: AC-3 propagation + SCC fixed-point.
//!
//! Resolves type constraints by propagating known types through the
//! constraint graph until all variables are assigned. For acyclic
//! grammars, a single AC-3 pass suffices. Cyclic grammars use
//! fixed-point iteration over SCCs — the same convergence pattern
//! as FIRST set computation.

use std::collections::VecDeque;

use crate::TypeDesc;

use super::constraint::*;
use super::generate::ConstraintSystem;
use super::utils::try_flatten_pair;

/// Solve all constraints, assigning a TypeDesc to each TypeVar.
///
/// AC-3 propagation: process constraints in worklist order.
/// When a variable's type is determined, re-process constraints
/// that depend on it. Converges in O(n) for acyclic constraint
/// graphs; O(n·k) for cyclic (k = max SCC iterations, typically 2-3).
pub fn solve(system: &mut ConstraintSystem) {
    let mut worklist: VecDeque<usize> = (0..system.constraints.len()).collect();

    // AC-3 main loop: process constraints until worklist is empty.
    let mut iterations = 0;
    const MAX_ITERATIONS: usize = 100_000;

    while let Some(constraint_idx) = worklist.pop_front() {
        iterations += 1;
        if iterations > MAX_ITERATIONS {
            break; // Safety bound — should never hit in practice
        }

        let changed = revise(&system.constraints[constraint_idx], &mut system.vars);
        if changed {
            // Re-enqueue constraints that depend on the changed variable.
            // For simplicity, re-enqueue all constraints — AC-3 is idempotent.
            // A production implementation would maintain dependency lists.
            for i in 0..system.constraints.len() {
                if i != constraint_idx
                    && depends_on(&system.constraints[i], &system.constraints[constraint_idx])
                {
                    worklist.push_back(i);
                }
            }
        }
    }
}

/// Revise a single constraint. Returns true if any variable's type changed.
///
/// AC-3 terminology: "revise" checks if an arc is consistent and prunes
/// the domain if not. In our deterministic setting, "prune" means
/// "assign the type".
fn revise(constraint: &TypeConstraint, vars: &mut [TypeVar]) -> bool {
    match constraint {
        TypeConstraint::Ground { var, ty } => assign(vars, *var, ty.clone()),

        TypeConstraint::Equal { target, source } => {
            if let Some(ty) = vars[*source as usize].solved.clone() {
                assign(vars, *target, ty)
            } else if let Some(ty) = vars[*target as usize].solved.clone() {
                assign(vars, *source, ty)
            } else {
                false // Both unsolved — wait
            }
        }

        TypeConstraint::Seq {
            var,
            children,
            preserve_spans,
            sp_override_originals,
            collapse_simple_spans,
            child_node_kinds,
        } => {
            // All children must be solved.
            let child_types: Vec<TypeDesc> = children
                .iter()
                .filter_map(|&c| vars[c as usize].solved.clone())
                .collect();

            if child_types.len() != children.len() {
                return false; // Not all children solved yet
            }

            // Span-method safety guard: when all children are Span after override,
            // decide whether to keep override (all-Span → Span) or revert.
            let all_span = child_types.iter().all(|t| *t == TypeDesc::Span);
            let all_simple_span = all_span
                && *collapse_simple_spans
                && !preserve_spans
                && child_node_kinds.iter().zip(child_types.iter()).all(
                    |(kind, ty)| {
                        match kind {
                            // Optional(Span) produces Option<Span> at runtime, not Span.
                            SeqChildKind::Optional => false,
                            // Span-method overridden Ref — codegen will call _sp().
                            SeqChildKind::SpOverrideRef => true,
                            // Naturally Span — leaf or collapsed expression.
                            SeqChildKind::Other => *ty == TypeDesc::Span,
                        }
                    },
                );

            let effective_types = if all_span && !all_simple_span {
                // Revert span-method overrides: use original (non-override) child types.
                children
                    .iter()
                    .zip(sp_override_originals.iter())
                    .map(|(child_var, orig)| {
                        if let Some(orig_var) = orig {
                            // Use the original (non-overridden) type.
                            vars[*orig_var as usize]
                                .solved
                                .clone()
                                .unwrap_or(TypeDesc::Span)
                        } else {
                            vars[*child_var as usize]
                                .solved
                                .clone()
                                .unwrap_or(TypeDesc::Span)
                        }
                    })
                    .collect::<Vec<_>>()
            } else {
                child_types
            };

            let preserve = *preserve_spans
                && effective_types.iter().all(|t| *t == TypeDesc::Span);

            let result = project_seq_type(&effective_types, preserve);
            assign(vars, *var, result)
        }

        TypeConstraint::Alt { var, branches } => {
            // All branches must be solved.
            let branch_types: Vec<TypeDesc> = branches
                .iter()
                .filter_map(|&b| vars[b as usize].solved.clone())
                .collect();

            if branch_types.len() != branches.len() {
                return false;
            }

            let result = join_types(&branch_types);
            assign(vars, *var, result)
        }

        TypeConstraint::AltInVec {
            var,
            branches,
            standard_var,
        } => {
            // All vec-context branch types must be solved.
            let branch_types: Vec<TypeDesc> = branches
                .iter()
                .filter_map(|&b| vars[b as usize].solved.clone())
                .collect();

            if branch_types.len() != branches.len() {
                return false;
            }

            let first = &branch_types[0];
            let all_same = branch_types.iter().all(|t| t == first);
            if all_same {
                assign(vars, *var, first.clone())
            } else {
                // Heterogeneous even with in_vec — fall back to standard Alt projection.
                if let Some(std_ty) = vars[*standard_var as usize].solved.clone() {
                    assign(vars, *var, std_ty)
                } else {
                    false // Standard var not yet solved
                }
            }
        }

        TypeConstraint::Optional {
            var,
            inner,
            transparent_ref,
        } => {
            if let Some(inner_ty) = vars[*inner as usize].solved.clone() {
                let result = if inner_ty == TypeDesc::Span {
                    TypeDesc::Span // Optional Span collapses
                } else if *transparent_ref {
                    // Transparent refs get unboxed in Optional context.
                    TypeDesc::Option(Box::new(TypeDesc::Enum))
                } else {
                    TypeDesc::Option(Box::new(inner_ty))
                };
                assign(vars, *var, result)
            } else {
                false
            }
        }

        TypeConstraint::Repeat { var, inner } => {
            if let Some(inner_ty) = vars[*inner as usize].solved.clone() {
                let result = if inner_ty == TypeDesc::Span {
                    TypeDesc::Span // Vec<Span> collapses
                } else {
                    TypeDesc::Vec(Box::new(inner_ty))
                };
                assign(vars, *var, result)
            } else {
                false
            }
        }

        TypeConstraint::Project { var, kept } => {
            if let Some(ty) = vars[*kept as usize].solved.clone() {
                assign(vars, *var, ty)
            } else {
                false
            }
        }

        TypeConstraint::Map { var, return_type } => {
            // Forward checking: @host return type directly constrains the variable.
            assign(vars, *var, return_type.clone())
        }
    }
}

/// Assign a type to a variable. Returns true if the type changed.
fn assign(vars: &mut [TypeVar], var: TypeVarId, ty: TypeDesc) -> bool {
    let slot = &mut vars[var as usize].solved;
    if slot.as_ref() == Some(&ty) {
        false // No change
    } else {
        *slot = Some(ty);
        true
    }
}

/// Check if constraint `a` depends on any variable that constraint `b` writes.
fn depends_on(a: &TypeConstraint, b: &TypeConstraint) -> bool {
    let written_by_b = written_vars(b);
    reads(a)
        .iter()
        .any(|r| written_by_b.iter().any(|w| *w == *r))
}

/// Get the variables written by a constraint.
fn written_vars(c: &TypeConstraint) -> Vec<TypeVarId> {
    match c {
        TypeConstraint::Ground { var, .. }
        | TypeConstraint::Seq { var, .. }
        | TypeConstraint::Alt { var, .. }
        | TypeConstraint::AltInVec { var, .. }
        | TypeConstraint::Optional { var, .. }
        | TypeConstraint::Repeat { var, .. }
        | TypeConstraint::Project { var, .. }
        | TypeConstraint::Map { var, .. } => vec![*var],
        TypeConstraint::Equal { target, source } => vec![*target, *source],
    }
}

/// Get all variables read by a constraint.
fn reads(c: &TypeConstraint) -> Vec<TypeVarId> {
    match c {
        TypeConstraint::Ground { .. } | TypeConstraint::Map { .. } => vec![],
        TypeConstraint::Equal { target, source } => vec![*target, *source],
        TypeConstraint::Seq {
            children,
            sp_override_originals,
            ..
        } => {
            let mut r: Vec<TypeVarId> = children.clone();
            for orig in sp_override_originals {
                if let Some(v) = orig {
                    r.push(*v);
                }
            }
            r
        }
        TypeConstraint::Alt { branches, .. } => branches.clone(),
        TypeConstraint::AltInVec {
            branches,
            standard_var,
            ..
        } => {
            let mut r = branches.clone();
            r.push(*standard_var);
            r
        }
        TypeConstraint::Optional { inner, .. } | TypeConstraint::Repeat { inner, .. } => {
            vec![*inner]
        }
        TypeConstraint::Project { kept, .. } => vec![*kept],
    }
}

// ── Type computation helpers ────────────────────────────────────────────

/// Compute the type of a Seq node from its children's types.
///
/// Applies Span compression: consecutive Span children collapse to a single Span.
/// Applies try_flatten_pair: (T, Vec<T)) → Vec<T).
fn project_seq_type(child_types: &[TypeDesc], preserve_spans: bool) -> TypeDesc {
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
                // else: consecutive Span — skip
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
            // Try flatten: (T, Vec<T)) → Vec<T)
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
/// CSP subtype constraint: if all branches have the same type, use it.
/// Otherwise, produce BoxedEnum (heterogeneous alternation).
fn join_types(branch_types: &[TypeDesc]) -> TypeDesc {
    if branch_types.is_empty() {
        return TypeDesc::Tuple(vec![]);
    }
    let first = &branch_types[0];
    if branch_types.iter().all(|t| t == first) {
        first.clone() // Homogeneous
    } else {
        TypeDesc::BoxedEnum // Heterogeneous — needs sub-variants
    }
}
