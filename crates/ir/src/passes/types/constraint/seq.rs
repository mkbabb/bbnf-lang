//! `SeqConstraint` — Seq projection with span compression and span-method
//! override revert. `SeqChildKind` records per-child kind context.

use csp_solver::constraint::{Constraint, Revision, VarId};
use csp_solver::variable::Variable;

use crate::TypeDesc;

use super::TypeVarId;
use super::domain::TypeDomain;
use super::helpers::{assign, project_seq_type};

/// Metadata about a Seq child for span-method override revert decisions.
#[derive(Clone, Debug)]
pub enum SeqChildKind {
    /// A Ref child with span-method override active.
    SpOverrideRef,
    /// An Optional (Repeat 0..1) child.
    Optional,
    /// Any other node.
    Other,
}

/// Seq constraint: computes the type of a sequence from its children.
/// After all children resolve, computes the Seq type (with Span compression).
#[derive(Debug)]
pub struct SeqConstraint {
    scope: Vec<VarId>,
    pub var: TypeVarId,
    pub children: Vec<TypeVarId>,
    pub preserve_spans: bool,
    pub sp_override_originals: Vec<Option<TypeVarId>>,
    pub collapse_simple_spans: bool,
    pub child_node_kinds: Vec<SeqChildKind>,
}

impl SeqConstraint {
    pub fn new(
        var: TypeVarId,
        children: Vec<TypeVarId>,
        preserve_spans: bool,
        sp_override_originals: Vec<Option<TypeVarId>>,
        collapse_simple_spans: bool,
        child_node_kinds: Vec<SeqChildKind>,
    ) -> Self {
        let mut scope = vec![var];
        scope.extend_from_slice(&children);
        for orig in &sp_override_originals {
            if let Some(v) = orig {
                scope.push(*v);
            }
        }
        // Deduplicate scope (VarId is u32, dedup is fine).
        scope.sort_unstable();
        scope.dedup();

        Self {
            scope,
            var,
            children,
            preserve_spans,
            sp_override_originals,
            collapse_simple_spans,
            child_node_kinds,
        }
    }
}

impl Constraint<TypeDomain> for SeqConstraint {
    fn scope(&self) -> &[VarId] {
        &self.scope
    }

    fn check(&self, _assignment: &[Option<Option<TypeDesc>>]) -> bool {
        true // Always satisfiable in our deterministic setting.
    }

    fn revise(&self, vars: &mut [Variable<TypeDomain>], _depth: usize) -> Revision {
        // All children must be solved.
        let child_types: Vec<TypeDesc> = self
            .children
            .iter()
            .filter_map(|&c| vars[c as usize].domain.solved.clone())
            .collect();

        if child_types.len() != self.children.len() {
            return Revision::Unchanged; // Not all children solved yet
        }

        // Span-method safety guard.
        let all_span = child_types.iter().all(|t| *t == TypeDesc::Span);
        let all_simple_span = all_span
            && self.collapse_simple_spans
            && !self.preserve_spans
            && self
                .child_node_kinds
                .iter()
                .zip(child_types.iter())
                .all(|(kind, ty)| match kind {
                    SeqChildKind::Optional => false,
                    SeqChildKind::SpOverrideRef => true,
                    SeqChildKind::Other => *ty == TypeDesc::Span,
                });

        let effective_types = if all_span && !all_simple_span {
            // Revert span-method overrides: use original (non-override) child types.
            self.children
                .iter()
                .zip(self.sp_override_originals.iter())
                .map(|(child_var, orig)| {
                    if let Some(orig_var) = orig {
                        vars[*orig_var as usize]
                            .domain
                            .solved
                            .clone()
                            .unwrap_or(TypeDesc::Span)
                    } else {
                        vars[*child_var as usize]
                            .domain
                            .solved
                            .clone()
                            .unwrap_or(TypeDesc::Span)
                    }
                })
                .collect::<Vec<_>>()
        } else {
            child_types
        };

        let preserve = self.preserve_spans
            && effective_types.iter().all(|t| *t == TypeDesc::Span);

        let result = project_seq_type(&effective_types, preserve);
        if assign(vars, self.var, result) {
            Revision::Changed
        } else {
            Revision::Unchanged
        }
    }
}
