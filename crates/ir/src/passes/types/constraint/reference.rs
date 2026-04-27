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
//! `Named`), the Ref falls back to `BoxedEnum` — the historical
//! value — because compound Ref positions are wrapped in enum
//! variants for tagged-union codegen.
//!
//! The constraint relies on the solver's fixed-point iteration
//! (LLVM-style `Changed` bool) to converge: if the target rule's
//! constraints order the Ref ahead of the target's body, the Ref's
//! `var` stays unsolved for that revise pass; the next pass (once
//! the target rule's body has assigned its var) fires propagation.

use csp_solver::constraint::{Constraint, Revision, VarId};
use csp_solver::variable::Variable;

use crate::TypeDesc;

use super::TypeVarId;
use super::domain::TypeDomain;
use super::revise::assign;

/// Ref constraint: the reference's type equals the target rule's
/// type when scalar, else `BoxedEnum`.
///
/// Scalar types are the ones with an inline payload slot (see
/// [`TypeDesc::needs_payload_slot`]) plus [`TypeDesc::Span`] — the
/// latter is a `(lo, hi)` pair stored natively in `TapeRec` and
/// equally eligible for compositional projection through a Seq of
/// annotated Refs.
#[derive(Debug)]
pub struct RefConstraint {
    scope: [VarId; 2],
    pub var: TypeVarId,
    pub rule_var: TypeVarId,
}

impl RefConstraint {
    pub fn new(var: TypeVarId, rule_var: TypeVarId) -> Self {
        Self {
            scope: [var, rule_var],
            var,
            rule_var,
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
        let result = {
            let rule_slot = &vars[self.rule_var as usize].domain.solved;
            match rule_slot {
                // Target rule not solved yet — wait for the next pass.
                None => return Revision::Unchanged,
                Some(ty) if is_scalar_projectable(ty) => ty.clone(),
                Some(_) => TypeDesc::BoxedEnum,
            }
        };

        if assign(vars, self.var, result) {
            Revision::Changed
        } else {
            Revision::Unchanged
        }
    }
}

/// True when the referencing node should inherit the target rule's
/// type directly rather than collapse to `BoxedEnum`.
///
/// Inherits for `Span` and every `needs_payload_slot()` primitive.
/// All compound types (`Tuple`, `Vec`, `Option`, `Named`) and the
/// enum wrappers remain `BoxedEnum` — the structural Ref position
/// is still holding an enum variant in those cases.
fn is_scalar_projectable(ty: &TypeDesc) -> bool {
    matches!(ty, TypeDesc::Span) || ty.needs_payload_slot()
}
