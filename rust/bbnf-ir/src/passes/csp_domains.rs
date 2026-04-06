//! CSP domain and constraint types for fixed-point passes.
//!
//! Provides two lattice domains:
//! - `BoolDomain`: top-down boolean lattice for span eligibility (starts `true`, can become `false`)
//! - `CharSetDomain`: monotonically growing `CharSet128` lattice for FIRST/FOLLOW sets
//!
//! Each domain implements `csp_solver::domain::{Domain, LatticeDomain}` and comes
//! with a set of constraint structs implementing `csp_solver::constraint::Constraint<D>`.

use csp_solver::constraint::{Constraint, Revision, VarId};
use csp_solver::domain::{Domain, LatticeDomain};
use csp_solver::variable::Variable;

use crate::CharSet128;

// ── BoolDomain ────────────────────────────────────────────────────────────────

/// Boolean lattice domain for span eligibility analysis.
///
/// - `None` = unsolved (bottom)
/// - `Some(true)` = eligible
/// - `Some(false)` = not eligible
///
/// Once assigned, the value can be refined from `true` to `false` (top-down).
#[derive(Clone, Debug, PartialEq)]
pub struct BoolDomain {
    pub solved: Option<bool>,
}

impl BoolDomain {
    pub fn unsolved() -> Self {
        Self { solved: None }
    }

    pub fn ground(val: bool) -> Self {
        Self { solved: Some(val) }
    }
}

impl Domain for BoolDomain {
    type Value = Option<bool>;

    fn size(&self) -> usize {
        1
    }

    fn is_singleton(&self) -> bool {
        true
    }

    fn singleton_value(&self) -> Option<Self::Value> {
        Some(self.solved)
    }

    fn contains(&self, val: &Self::Value) -> bool {
        self.solved == *val
    }

    fn remove(&mut self, _val: &Self::Value) -> bool {
        false
    }

    fn add(&mut self, _val: &Self::Value) {}

    fn values(&self) -> Vec<Self::Value> {
        vec![self.solved]
    }
}

impl LatticeDomain for BoolDomain {
    fn bottom() -> Self {
        Self { solved: None }
    }

    fn join(&mut self, other: &Self) -> bool {
        match (&self.solved, &other.solved) {
            (None, Some(val)) => {
                self.solved = Some(*val);
                true
            }
            // Allow refinement: true -> false (top-down)
            (Some(true), Some(false)) => {
                self.solved = Some(false);
                true
            }
            _ => false,
        }
    }
}

// ── BoolDomain constraints ────────────────────────────────────────────────────

/// Ground constraint: assigns a fixed boolean to a variable.
#[derive(Debug)]
pub struct BoolGroundConstraint {
    scope: [VarId; 1],
    pub var: VarId,
    pub val: bool,
}

impl BoolGroundConstraint {
    pub fn new(var: VarId, val: bool) -> Self {
        Self {
            scope: [var],
            var,
            val,
        }
    }
}

impl Constraint<BoolDomain> for BoolGroundConstraint {
    fn scope(&self) -> &[VarId] {
        &self.scope
    }

    fn check(&self, assignment: &[Option<Option<bool>>]) -> bool {
        match &assignment[self.var as usize] {
            Some(Some(v)) => *v == self.val,
            _ => true,
        }
    }

    fn revise(&self, vars: &mut [Variable<BoolDomain>], _depth: usize) -> Revision {
        if bool_assign(vars, self.var, self.val) {
            Revision::Changed
        } else {
            Revision::Unchanged
        }
    }
}

/// Equal constraint: target gets the same value as source.
#[derive(Debug)]
pub struct BoolEqualConstraint {
    scope: [VarId; 2],
    pub target: VarId,
    pub source: VarId,
}

impl BoolEqualConstraint {
    pub fn new(target: VarId, source: VarId) -> Self {
        Self {
            scope: [target, source],
            target,
            source,
        }
    }
}

impl Constraint<BoolDomain> for BoolEqualConstraint {
    fn scope(&self) -> &[VarId] {
        &self.scope
    }

    fn check(&self, assignment: &[Option<Option<bool>>]) -> bool {
        match (&assignment[self.target as usize], &assignment[self.source as usize]) {
            (Some(a), Some(b)) => a == b,
            _ => true,
        }
    }

    fn revise(&self, vars: &mut [Variable<BoolDomain>], _depth: usize) -> Revision {
        let source_val = vars[self.source as usize].domain.solved;
        let target_val = vars[self.target as usize].domain.solved;

        let mut changed = false;
        if let Some(v) = source_val {
            changed |= bool_assign(vars, self.target, v);
        } else if let Some(v) = target_val {
            changed |= bool_assign(vars, self.source, v);
        }

        if changed {
            Revision::Changed
        } else {
            Revision::Unchanged
        }
    }
}

/// And constraint: target = all sources are true.
/// If any source is false, target becomes false.
/// If all sources are true, target becomes true.
#[derive(Debug)]
pub struct BoolAndConstraint {
    scope: Vec<VarId>,
    pub var: VarId,
    pub sources: Vec<VarId>,
}

impl BoolAndConstraint {
    pub fn new(var: VarId, sources: Vec<VarId>) -> Self {
        let mut scope = vec![var];
        scope.extend_from_slice(&sources);
        scope.sort_unstable();
        scope.dedup();
        Self { scope, var, sources }
    }
}

impl Constraint<BoolDomain> for BoolAndConstraint {
    fn scope(&self) -> &[VarId] {
        &self.scope
    }

    fn check(&self, _assignment: &[Option<Option<bool>>]) -> bool {
        true
    }

    fn revise(&self, vars: &mut [Variable<BoolDomain>], _depth: usize) -> Revision {
        // If any source is false, result is false immediately.
        for &src in &self.sources {
            if vars[src as usize].domain.solved == Some(false) {
                return if bool_assign(vars, self.var, false) {
                    Revision::Changed
                } else {
                    Revision::Unchanged
                };
            }
        }

        // If all sources are solved and true, result is true.
        let all_solved = self
            .sources
            .iter()
            .all(|&s| vars[s as usize].domain.solved.is_some());
        if all_solved {
            let all_true = self
                .sources
                .iter()
                .all(|&s| vars[s as usize].domain.solved == Some(true));
            if bool_assign(vars, self.var, all_true) {
                return Revision::Changed;
            }
        }

        Revision::Unchanged
    }
}

/// Assign a boolean to a variable's domain. Returns true if changed.
fn bool_assign(vars: &mut [Variable<BoolDomain>], var: VarId, val: bool) -> bool {
    let slot = &mut vars[var as usize].domain.solved;
    if *slot == Some(val) {
        false
    } else {
        *slot = Some(val);
        true
    }
}

// ── CharSetDomain ─────────────────────────────────────────────────────────────

/// Monotonically growing `CharSet128` lattice domain for FIRST/FOLLOW sets.
///
/// Bottom = empty set. Join = union. Values only grow.
#[derive(Clone, Debug, PartialEq)]
pub struct CharSetDomain {
    pub solved: CharSet128,
}

impl CharSetDomain {
    pub fn empty() -> Self {
        Self {
            solved: CharSet128::new(),
        }
    }

    pub fn from(cs: CharSet128) -> Self {
        Self { solved: cs }
    }
}

impl Domain for CharSetDomain {
    type Value = CharSet128;

    fn size(&self) -> usize {
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
        false
    }

    fn add(&mut self, _val: &Self::Value) {}

    fn values(&self) -> Vec<Self::Value> {
        vec![self.solved.clone()]
    }
}

impl LatticeDomain for CharSetDomain {
    fn bottom() -> Self {
        Self::empty()
    }

    fn join(&mut self, other: &Self) -> bool {
        let old = self.solved.bits;
        self.solved.union(&other.solved);
        self.solved.bits != old
    }
}

// ── CharSetDomain constraints ─────────────────────────────────────────────────

/// Ground constraint: union a fixed CharSet128 into a variable.
#[derive(Debug)]
pub struct CharSetGroundConstraint {
    scope: [VarId; 1],
    pub var: VarId,
    pub chars: CharSet128,
}

impl CharSetGroundConstraint {
    pub fn new(var: VarId, chars: CharSet128) -> Self {
        Self {
            scope: [var],
            var,
            chars,
        }
    }
}

impl Constraint<CharSetDomain> for CharSetGroundConstraint {
    fn scope(&self) -> &[VarId] {
        &self.scope
    }

    fn check(&self, _assignment: &[Option<CharSet128>]) -> bool {
        true
    }

    fn revise(&self, vars: &mut [Variable<CharSetDomain>], _depth: usize) -> Revision {
        if charset_union(vars, self.var, &self.chars) {
            Revision::Changed
        } else {
            Revision::Unchanged
        }
    }
}

/// Union constraint: target |= source.
#[derive(Debug)]
pub struct CharSetUnionConstraint {
    scope: [VarId; 2],
    pub target: VarId,
    pub source: VarId,
}

impl CharSetUnionConstraint {
    pub fn new(target: VarId, source: VarId) -> Self {
        Self {
            scope: [target, source],
            target,
            source,
        }
    }
}

impl Constraint<CharSetDomain> for CharSetUnionConstraint {
    fn scope(&self) -> &[VarId] {
        &self.scope
    }

    fn check(&self, _assignment: &[Option<CharSet128>]) -> bool {
        true
    }

    fn revise(&self, vars: &mut [Variable<CharSetDomain>], _depth: usize) -> Revision {
        let source_cs = vars[self.source as usize].domain.solved.clone();
        if charset_union(vars, self.target, &source_cs) {
            Revision::Changed
        } else {
            Revision::Unchanged
        }
    }
}

/// Multi-union constraint: target |= source_0 | source_1 | ...
#[derive(Debug)]
pub struct CharSetMultiUnionConstraint {
    scope: Vec<VarId>,
    pub target: VarId,
    pub sources: Vec<VarId>,
}

impl CharSetMultiUnionConstraint {
    pub fn new(target: VarId, sources: Vec<VarId>) -> Self {
        let mut scope = vec![target];
        scope.extend_from_slice(&sources);
        scope.sort_unstable();
        scope.dedup();
        Self {
            scope,
            target,
            sources,
        }
    }
}

impl Constraint<CharSetDomain> for CharSetMultiUnionConstraint {
    fn scope(&self) -> &[VarId] {
        &self.scope
    }

    fn check(&self, _assignment: &[Option<CharSet128>]) -> bool {
        true
    }

    fn revise(&self, vars: &mut [Variable<CharSetDomain>], _depth: usize) -> Revision {
        let mut combined = CharSet128::new();
        for &src in &self.sources {
            combined.union(&vars[src as usize].domain.solved);
        }
        if charset_union(vars, self.target, &combined) {
            Revision::Changed
        } else {
            Revision::Unchanged
        }
    }
}

/// SeqFirst constraint for FIRST set computation:
/// target = FIRST(child_0) | (if child_0 nullable: FIRST(child_1) | ...)
///
/// Takes child variables and a precomputed nullable flag per child.
#[derive(Debug)]
pub struct CharSetSeqFirstConstraint {
    scope: Vec<VarId>,
    pub target: VarId,
    pub children: Vec<VarId>,
    /// Per-child nullable flag (precomputed, not a variable).
    pub nullable: Vec<bool>,
}

impl CharSetSeqFirstConstraint {
    pub fn new(target: VarId, children: Vec<VarId>, nullable: Vec<bool>) -> Self {
        let mut scope = vec![target];
        scope.extend_from_slice(&children);
        scope.sort_unstable();
        scope.dedup();
        Self {
            scope,
            target,
            children,
            nullable,
        }
    }
}

impl Constraint<CharSetDomain> for CharSetSeqFirstConstraint {
    fn scope(&self) -> &[VarId] {
        &self.scope
    }

    fn check(&self, _assignment: &[Option<CharSet128>]) -> bool {
        true
    }

    fn revise(&self, vars: &mut [Variable<CharSetDomain>], _depth: usize) -> Revision {
        let mut combined = CharSet128::new();
        for (i, &child_var) in self.children.iter().enumerate() {
            combined.union(&vars[child_var as usize].domain.solved);
            if !self.nullable[i] {
                break;
            }
        }
        if charset_union(vars, self.target, &combined) {
            Revision::Changed
        } else {
            Revision::Unchanged
        }
    }
}

/// Union a CharSet128 into a variable's domain. Returns true if changed.
fn charset_union(vars: &mut [Variable<CharSetDomain>], var: VarId, cs: &CharSet128) -> bool {
    let old = vars[var as usize].domain.solved.bits;
    vars[var as usize].domain.solved.union(cs);
    vars[var as usize].domain.solved.bits != old
}
