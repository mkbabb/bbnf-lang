//! CSP type constraint definitions using the generalized `csp-solver` crate.
//!
//! Models type inference as a Constraint Satisfaction Problem:
//! - **Variables**: One per IR node, domain is `TypeDomain` (wrapping `Option<TypeDesc>`)
//! - **Constraints**: Type equations/inequalities from grammar structure
//! - **Solver**: AC-3 propagation via `csp_solver::Csp::propagate()`
//!
//! The domain of each variable is a single `TypeDesc` value (deterministic --
//! no search/backtracking needed). AC-3 propagation resolves all constraints
//! in a single pass for acyclic grammars; cyclic grammars converge via
//! fixed-point iteration.

use csp_solver::constraint::{Constraint, Revision, VarId};
use csp_solver::domain::{Domain, LatticeDomain};
use csp_solver::variable::Variable;

use crate::TypeDesc;

use super::utils::try_flatten_pair;

// Re-export VarId from the csp crate as the canonical type variable ID.
pub type TypeVarId = VarId;

// ── TypeDomain ─────────────────────────────────────────────────────────────────

/// Domain for type inference variables: wraps `Option<TypeDesc>`.
///
/// Starts as `None` (bottom/unsolved). Once assigned to `Some(ty)`, the
/// domain is a singleton. The lattice `join` assigns the type if currently
/// bottom, matching the deterministic "assign-once" semantics of the
/// bespoke solver.
#[derive(Clone, Debug, PartialEq)]
pub struct TypeDomain {
    pub solved: Option<TypeDesc>,
}

impl TypeDomain {
    pub fn unsolved() -> Self {
        Self { solved: None }
    }

    pub fn ground(ty: TypeDesc) -> Self {
        Self { solved: Some(ty) }
    }
}

impl Domain for TypeDomain {
    type Value = Option<TypeDesc>;

    fn size(&self) -> usize {
        // Lattice domain: always a "singleton" from the solver's perspective.
        1
    }

    fn is_singleton(&self) -> bool {
        true
    }

    fn singleton_value(&self) -> Option<Self::Value> {
        Some(self.solved.clone())
    }

    fn contains(&self, val: &Self::Value) -> bool {
        self.solved == *val
    }

    fn remove(&mut self, _val: &Self::Value) -> bool {
        // Lattice domains don't support removal -- they only grow.
        false
    }

    fn add(&mut self, _val: &Self::Value) {
        // No-op for lattice domains.
    }

    fn values(&self) -> Vec<Self::Value> {
        vec![self.solved.clone()]
    }
}

impl LatticeDomain for TypeDomain {
    fn bottom() -> Self {
        Self { solved: None }
    }

    fn join(&mut self, other: &Self) -> bool {
        match (&self.solved, &other.solved) {
            (None, Some(ty)) => {
                self.solved = Some(ty.clone());
                true
            }
            (Some(existing), Some(ty)) if existing != ty => {
                // Re-assignment to a different type: allow it (the solver
                // may refine types during convergence).
                self.solved = Some(ty.clone());
                true
            }
            _ => false,
        }
    }
}

// ── Metadata about Seq children ────────────────────────────────────────────────

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

// ── Constraint implementations ─────────────────────────────────────────────────

/// Ground constraint: assigns a fixed type to a variable.
/// AC-3: singleton domain, immediately resolved.
#[derive(Debug)]
pub struct GroundConstraint {
    scope: [VarId; 1],
    pub var: TypeVarId,
    pub ty: TypeDesc,
}

impl GroundConstraint {
    pub fn new(var: TypeVarId, ty: TypeDesc) -> Self {
        Self {
            scope: [var],
            var,
            ty,
        }
    }
}

impl Constraint<TypeDomain> for GroundConstraint {
    fn scope(&self) -> &[VarId] {
        &self.scope
    }

    fn check(&self, assignment: &[Option<Option<TypeDesc>>]) -> bool {
        match &assignment[self.var as usize] {
            Some(Some(ty)) => *ty == self.ty,
            _ => true,
        }
    }

    fn revise(&self, vars: &mut [Variable<TypeDomain>], _depth: usize) -> Revision {
        if assign(vars, self.var, self.ty.clone()) {
            Revision::Changed
        } else {
            Revision::Unchanged
        }
    }
}

/// Equal constraint: two variables must have the same type.
/// Bidirectional: when either resolves, the other does too.
#[derive(Debug)]
pub struct EqualConstraint {
    scope: [VarId; 2],
    pub target: TypeVarId,
    pub source: TypeVarId,
}

impl EqualConstraint {
    pub fn new(target: TypeVarId, source: TypeVarId) -> Self {
        Self {
            scope: [target, source],
            target,
            source,
        }
    }
}

impl Constraint<TypeDomain> for EqualConstraint {
    fn scope(&self) -> &[VarId] {
        &self.scope
    }

    fn check(&self, assignment: &[Option<Option<TypeDesc>>]) -> bool {
        match (&assignment[self.target as usize], &assignment[self.source as usize]) {
            (Some(a), Some(b)) => a == b,
            _ => true,
        }
    }

    fn revise(&self, vars: &mut [Variable<TypeDomain>], _depth: usize) -> Revision {
        let source_ty = vars[self.source as usize].domain.solved.clone();
        let target_ty = vars[self.target as usize].domain.solved.clone();

        let mut changed = false;
        if let Some(ty) = source_ty {
            changed |= assign(vars, self.target, ty);
        } else if let Some(ty) = target_ty {
            changed |= assign(vars, self.source, ty);
        }

        if changed {
            Revision::Changed
        } else {
            Revision::Unchanged
        }
    }
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

/// Alt constraint: join (least upper bound) of alternation branch types.
#[derive(Debug)]
pub struct AltConstraint {
    scope: Vec<VarId>,
    pub var: TypeVarId,
    pub branches: Vec<TypeVarId>,
}

impl AltConstraint {
    pub fn new(var: TypeVarId, branches: Vec<TypeVarId>) -> Self {
        let mut scope = vec![var];
        scope.extend_from_slice(&branches);
        scope.sort_unstable();
        scope.dedup();
        Self {
            scope,
            var,
            branches,
        }
    }
}

impl Constraint<TypeDomain> for AltConstraint {
    fn scope(&self) -> &[VarId] {
        &self.scope
    }

    fn check(&self, _assignment: &[Option<Option<TypeDesc>>]) -> bool {
        true
    }

    fn revise(&self, vars: &mut [Variable<TypeDomain>], _depth: usize) -> Revision {
        let branch_types: Vec<TypeDesc> = self
            .branches
            .iter()
            .filter_map(|&b| vars[b as usize].domain.solved.clone())
            .collect();

        if branch_types.len() != self.branches.len() {
            return Revision::Unchanged;
        }

        let result = join_types(&branch_types);
        if assign(vars, self.var, result) {
            Revision::Changed
        } else {
            Revision::Unchanged
        }
    }
}

/// AltInVec constraint: alternation in Vec context.
/// Uses in-vec projection for branch types; falls back to standard Alt
/// projection when branches are heterogeneous.
#[derive(Debug)]
pub struct AltInVecConstraint {
    scope: Vec<VarId>,
    pub var: TypeVarId,
    pub branches: Vec<TypeVarId>,
    pub standard_var: TypeVarId,
}

impl AltInVecConstraint {
    pub fn new(var: TypeVarId, branches: Vec<TypeVarId>, standard_var: TypeVarId) -> Self {
        let mut scope = vec![var, standard_var];
        scope.extend_from_slice(&branches);
        scope.sort_unstable();
        scope.dedup();
        Self {
            scope,
            var,
            branches,
            standard_var,
        }
    }
}

impl Constraint<TypeDomain> for AltInVecConstraint {
    fn scope(&self) -> &[VarId] {
        &self.scope
    }

    fn check(&self, _assignment: &[Option<Option<TypeDesc>>]) -> bool {
        true
    }

    fn revise(&self, vars: &mut [Variable<TypeDomain>], _depth: usize) -> Revision {
        let branch_types: Vec<TypeDesc> = self
            .branches
            .iter()
            .filter_map(|&b| vars[b as usize].domain.solved.clone())
            .collect();

        if branch_types.len() != self.branches.len() {
            return Revision::Unchanged;
        }

        let first = &branch_types[0];
        let all_same = branch_types.iter().all(|t| t == first);

        if all_same {
            if assign(vars, self.var, first.clone()) {
                return Revision::Changed;
            }
        } else {
            // Heterogeneous -- fall back to standard Alt projection.
            if let Some(std_ty) = vars[self.standard_var as usize].domain.solved.clone() {
                if assign(vars, self.var, std_ty) {
                    return Revision::Changed;
                }
            }
            // Standard var not yet solved -- wait.
        }

        Revision::Unchanged
    }
}

/// Optional constraint: wraps inner type in `Option<T>`.
/// Optional Span collapses to Span. Transparent refs get Option<Enum>.
#[derive(Debug)]
pub struct OptionalConstraint {
    scope: [VarId; 2],
    pub var: TypeVarId,
    pub inner: TypeVarId,
    pub transparent_ref: bool,
}

impl OptionalConstraint {
    pub fn new(var: TypeVarId, inner: TypeVarId, transparent_ref: bool) -> Self {
        Self {
            scope: [var, inner],
            var,
            inner,
            transparent_ref,
        }
    }
}

impl Constraint<TypeDomain> for OptionalConstraint {
    fn scope(&self) -> &[VarId] {
        &self.scope
    }

    fn check(&self, _assignment: &[Option<Option<TypeDesc>>]) -> bool {
        true
    }

    fn revise(&self, vars: &mut [Variable<TypeDomain>], _depth: usize) -> Revision {
        if let Some(inner_ty) = vars[self.inner as usize].domain.solved.clone() {
            let result = if inner_ty == TypeDesc::Span {
                TypeDesc::Span
            } else if self.transparent_ref {
                TypeDesc::Option(Box::new(TypeDesc::Enum))
            } else {
                TypeDesc::Option(Box::new(inner_ty))
            };
            if assign(vars, self.var, result) {
                Revision::Changed
            } else {
                Revision::Unchanged
            }
        } else {
            Revision::Unchanged
        }
    }
}

/// Repeat constraint: wraps inner type in `Vec<T>`.
/// Vec<Span> collapses to Span.
#[derive(Debug)]
pub struct RepeatConstraint {
    scope: [VarId; 2],
    pub var: TypeVarId,
    pub inner: TypeVarId,
}

impl RepeatConstraint {
    pub fn new(var: TypeVarId, inner: TypeVarId) -> Self {
        Self {
            scope: [var, inner],
            var,
            inner,
        }
    }
}

impl Constraint<TypeDomain> for RepeatConstraint {
    fn scope(&self) -> &[VarId] {
        &self.scope
    }

    fn check(&self, _assignment: &[Option<Option<TypeDesc>>]) -> bool {
        true
    }

    fn revise(&self, vars: &mut [Variable<TypeDomain>], _depth: usize) -> Revision {
        if let Some(inner_ty) = vars[self.inner as usize].domain.solved.clone() {
            let result = if inner_ty == TypeDesc::Span {
                TypeDesc::Span
            } else {
                TypeDesc::Vec(Box::new(inner_ty))
            };
            if assign(vars, self.var, result) {
                Revision::Changed
            } else {
                Revision::Unchanged
            }
        } else {
            Revision::Unchanged
        }
    }
}

/// Project constraint: keeps one side of Skip/Next/Minus.
#[derive(Debug)]
pub struct ProjectConstraint {
    scope: [VarId; 2],
    pub var: TypeVarId,
    pub kept: TypeVarId,
}

impl ProjectConstraint {
    pub fn new(var: TypeVarId, kept: TypeVarId) -> Self {
        Self {
            scope: [var, kept],
            var,
            kept,
        }
    }
}

impl Constraint<TypeDomain> for ProjectConstraint {
    fn scope(&self) -> &[VarId] {
        &self.scope
    }

    fn check(&self, _assignment: &[Option<Option<TypeDesc>>]) -> bool {
        true
    }

    fn revise(&self, vars: &mut [Variable<TypeDomain>], _depth: usize) -> Revision {
        if let Some(ty) = vars[self.kept as usize].domain.solved.clone() {
            if assign(vars, self.var, ty) {
                Revision::Changed
            } else {
                Revision::Unchanged
            }
        } else {
            Revision::Unchanged
        }
    }
}

/// Map constraint: overrides type from FnDescriptor return type.
#[derive(Debug)]
pub struct MapConstraint {
    scope: [VarId; 1],
    pub var: TypeVarId,
    pub return_type: TypeDesc,
}

impl MapConstraint {
    pub fn new(var: TypeVarId, return_type: TypeDesc) -> Self {
        Self {
            scope: [var],
            var,
            return_type,
        }
    }
}

impl Constraint<TypeDomain> for MapConstraint {
    fn scope(&self) -> &[VarId] {
        &self.scope
    }

    fn check(&self, assignment: &[Option<Option<TypeDesc>>]) -> bool {
        match &assignment[self.var as usize] {
            Some(Some(ty)) => *ty == self.return_type,
            _ => true,
        }
    }

    fn revise(&self, vars: &mut [Variable<TypeDomain>], _depth: usize) -> Revision {
        if assign(vars, self.var, self.return_type.clone()) {
            Revision::Changed
        } else {
            Revision::Unchanged
        }
    }
}

// ── Shared helpers ─────────────────────────────────────────────────────────────

/// Assign a type to a variable's domain. Returns true if the type changed.
fn assign(vars: &mut [Variable<TypeDomain>], var: TypeVarId, ty: TypeDesc) -> bool {
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
fn join_types(branch_types: &[TypeDesc]) -> TypeDesc {
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
