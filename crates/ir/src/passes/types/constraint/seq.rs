//! `SeqConstraint` — Seq projection with span compression and span-method
//! override revert. `SeqChildKind` records per-child kind context.

use csp_solver::constraint::{Constraint, Revision, VarId};
use csp_solver::variable::Variable;

use crate::TypeDesc;

use super::TypeVarId;
use super::domain::TypeDomain;
use super::revise::{assign, project_seq_type};

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
        // Tranche Y.10: compute the result while holding borrows into
        // `vars`, then drop the borrows before calling `assign`. This
        // collapses the previous 2N clones per revise into N clones
        // only where `project_seq_type` genuinely needs owned values
        // (Tuple elements) — and zero clones in the all-Span fast path.
        let result = {
            let child_types: Vec<&TypeDesc> = self
                .children
                .iter()
                .filter_map(|&c| vars[c as usize].domain.solved.as_ref())
                .collect();

            if child_types.len() != self.children.len() {
                return Revision::Unchanged; // Not all children solved yet
            }

            // Span-method safety guard.
            let all_span = child_types.iter().all(|t| **t == TypeDesc::Span);
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
                        SeqChildKind::Other => **ty == TypeDesc::Span,
                    });

            // If the all-Span override revert applies, we have to read
            // the `sp_override_originals` variables — still as borrows.
            // The Span placeholder is a unit variant so `.unwrap_or` on
            // a static avoids allocation entirely.
            const SPAN: TypeDesc = TypeDesc::Span;

            if all_span && !all_simple_span {
                let effective_refs: Vec<&TypeDesc> = self
                    .children
                    .iter()
                    .zip(self.sp_override_originals.iter())
                    .map(|(child_var, orig)| {
                        let target = orig.unwrap_or(*child_var);
                        vars[target as usize]
                            .domain
                            .solved
                            .as_ref()
                            .unwrap_or(&SPAN)
                    })
                    .collect();

                let preserve = self.preserve_spans
                    && effective_refs.iter().all(|t| **t == TypeDesc::Span);

                project_seq_type(&effective_refs, preserve)
            } else {
                let preserve = self.preserve_spans
                    && child_types.iter().all(|t| **t == TypeDesc::Span);

                project_seq_type(&child_types, preserve)
            }
        };

        if assign(vars, self.var, result) {
            Revision::Changed
        } else {
            Revision::Unchanged
        }
    }
}
