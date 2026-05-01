//! `RefConstraint` — scalar-propagating reference projection.
//!
//! `IrNode::Ref(rid)` historically ground-assigned `TypeDesc::BoxedEnum`
//! at every call site. That erased scalar types: `Seq(number, unit)`
//! with `number -> f64` and `unit -> u8` projected to
//! `Tuple([BoxedEnum, BoxedEnum])` instead of `Tuple([F64, U8])`,
//! and the aggregate-payload path never activated.
//!
//! `RefConstraint` instead bridges the referencing node's type
//! variable to the target rule's type variable. The target rule's
//! type is computed by the same CSP pass, so once it resolves the
//! Ref propagates the scalar through; when the target rule's type
//! is compound (`Tuple`, `Vec`, `Option`, `BoxedEnum`, `Enum`,
//! `Named`), the Ref is wrapped in `BoxedEnum` for tagged-union
//! codegen — but the wrapping is now NAMED via an
//! [`UnresolvedCompoundRef`](crate::passes::types::TypeObligation)
//! obligation rather than a silent fallback (per AZ-III invariant 7,
//! "no silent fallback").
//!
//! The constraint relies on the solver's fixed-point iteration
//! (LLVM-style `Changed` bool) to converge: if the target rule's
//! constraints order the Ref ahead of the target's body, the Ref's
//! `var` stays unsolved for that revise pass; the next pass (once
//! the target rule's body has assigned its var) fires propagation.

use csp_solver::constraint::{Constraint, Revision, VarId};
use csp_solver::variable::Variable;

use crate::TypeDesc;
use crate::dag::NodeId;
use crate::passes::types::{ObligationSink, TypeObligation};
use crate::types::RuleId;

use super::TypeVarId;
use super::domain::TypeDomain;
use super::revise::assign;

/// Ref constraint: the reference's type equals the target rule's
/// type when scalar, else `BoxedEnum` and a recorded obligation.
///
/// Scalar types are the ones with an inline payload slot (see
/// [`TypeDesc::needs_payload_slot`]) plus [`TypeDesc::Span`] — the
/// latter is a `(lo, hi)` pair stored natively in `TapeRec` and
/// equally eligible for compositional projection through a Seq of
/// annotated Refs.
///
/// Compound target rules (`Tuple`, `Vec`, `Option`, `Named`, `Enum`,
/// `BoxedEnum`) project as `BoxedEnum` because the Ref position
/// carries an enum variant in tagged-union codegen. That projection
/// is intentional and grammar-general; the obligation captures the
/// underlying compound shape so audit/registry/diagnostic consumers
/// can see why the Ref collapsed.
#[derive(Debug)]
pub struct RefConstraint {
    scope: [VarId; 2],
    pub var: TypeVarId,
    pub rule_var: TypeVarId,
    /// The Ref node's stable `NodeId` (used by the obligation
    /// diagnostic). Carries the tree position even though the CSP
    /// itself indexes by `TypeVarId`.
    ref_node: NodeId,
    /// The Ref's target `RuleId` (used by the obligation diagnostic).
    target_rule: RuleId,
    /// Shared obligation sink — every compound-Ref collapse records a
    /// [`TypeObligation::UnresolvedCompoundRef`] here so the solver
    /// driver can drain them once propagation converges.
    sink: ObligationSink,
}

impl RefConstraint {
    pub fn new(
        var: TypeVarId,
        rule_var: TypeVarId,
        ref_node: NodeId,
        target_rule: RuleId,
        sink: ObligationSink,
    ) -> Self {
        Self {
            scope: [var, rule_var],
            var,
            rule_var,
            ref_node,
            target_rule,
            sink,
        }
    }
}

impl Constraint<TypeDomain> for RefConstraint {
    fn scope(&self) -> &[VarId] {
        &self.scope
    }

    fn check(&self, _assignment: &[Option<Option<TypeDesc>>]) -> bool {
        true
    }

    fn revise(&self, vars: &mut [Variable<TypeDomain>], _depth: usize) -> Revision {
        // Snapshot the rule slot. Either:
        //   * `None` — target not solved this pass; suspend.
        //   * `Some(scalar)` — propagate scalar through.
        //   * `Some(compound)` — collapse to BoxedEnum and record
        //     a NAMED obligation so the fallback is no longer silent.
        let (result, obligation) = {
            let rule_slot = &vars[self.rule_var as usize].domain.solved;
            match rule_slot {
                // Target rule not solved yet — wait for the next pass.
                None => return Revision::Unchanged,
                Some(ty) if is_scalar_projectable(ty) => (ty.clone(), None),
                Some(structural) => {
                    let obligation = TypeObligation::UnresolvedCompoundRef {
                        ref_node: Some(self.ref_node),
                        target_rule: self.target_rule,
                        structural_type: structural.clone(),
                        cyclic: false,
                    };
                    (TypeDesc::BoxedEnum, Some(obligation))
                }
            }
        };

        if let Some(o) = obligation {
            self.sink.record(o);
        }

        if assign(vars, self.var, result) {
            Revision::Changed
        } else {
            Revision::Unchanged
        }
    }
}

/// True when the referencing node should inherit the target rule's
/// type directly rather than collapse to `BoxedEnum` and surface an
/// obligation.
///
/// Inherits for `Span` and every `needs_payload_slot()` primitive.
/// All compound types (`Tuple`, `Vec`, `Option`, `Named`) and the
/// enum wrappers remain `BoxedEnum` — the structural Ref position
/// is still holding an enum variant in those cases — and the
/// projection records a [`TypeObligation::UnresolvedCompoundRef`]
/// so the collapse is named.
fn is_scalar_projectable(ty: &TypeDesc) -> bool {
    matches!(ty, TypeDesc::Span) || ty.needs_payload_slot()
}
