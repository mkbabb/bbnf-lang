//! CSP builder surface — variable / constraint construction and `finalize()`.
//!
//! The [`Csp<D>`] container and its configuration vocabulary are defined in
//! [`crate::config`]; the solve / propagate dispatch in [`solve`].
//!
//! Tests: `tests/solver.rs` (builder + solve correctness, every `add_*` path).

pub mod solve;

use crate::adjacency::Adjacency;
use crate::config::{Csp, SolveStats};
use crate::constraint::{
    self, AllDifferent, Constraint, ConstraintEnum, NotEqual, SoftLambdaConstraint, VarId,
};
use crate::domain::Domain;
use crate::variable::Variable;

impl<D: Domain> Csp<D> {
    /// Create a new empty CSP.
    pub fn new() -> Self {
        Self {
            variables: Vec::new(),
            constraints: Vec::new(),
            adjacency: None,
            stats: SolveStats::default(),
            constraint_weights: Vec::new(),
            var_constraint_ids: Vec::new(),
        }
    }

    /// Add a variable with the given domain. Returns its VarId.
    pub fn add_variable(&mut self, domain: D) -> VarId {
        let id = self.variables.len() as VarId;
        self.variables.push(Variable::new(domain));
        id
    }

    /// Add multiple variables sharing the same domain. Returns their VarIds.
    pub fn add_variables(&mut self, domain: &D, count: usize) -> Vec<VarId> {
        (0..count)
            .map(|_| self.add_variable(domain.clone()))
            .collect()
    }

    /// Add a custom constraint (wrapped in the `Custom` enum variant).
    pub fn add_constraint(&mut self, c: impl Constraint<D> + 'static) {
        self.constraints.push(ConstraintEnum::Custom(Box::new(c)));
    }

    /// Add a pre-typed constraint enum directly (avoids boxing for built-in types).
    pub fn add_constraint_enum(&mut self, c: ConstraintEnum<D>) {
        self.constraints.push(c);
    }

    /// Add a soft constraint (contributes penalty cost when violated, never prunes).
    pub fn add_soft_constraint(&mut self, c: SoftLambdaConstraint<D>) {
        self.constraints.push(ConstraintEnum::Soft(c));
    }

    /// Add a not-equal constraint (devirtualized fast path).
    pub fn add_not_equal(&mut self, x: VarId, y: VarId) {
        self.constraints
            .push(ConstraintEnum::NotEqual(NotEqual::new(x, y)));
    }

    /// Add an all-different constraint (devirtualized fast path).
    pub fn add_all_different(&mut self, vars: Vec<VarId>) {
        self.constraints
            .push(ConstraintEnum::AllDifferent(AllDifferent::new(vars)));
    }

    /// Fix a variable to a specific value.
    pub fn add_equals(&mut self, var: VarId, value: D::Value)
    where
        D: 'static,
        D::Value: Send + Sync,
    {
        self.add_constraint(constraint::LambdaConstraint::new(
            vec![var],
            move |assignment| match &assignment[var as usize] {
                Some(v) => *v == value,
                None => true,
            },
            format!("equals({var})"),
        ));
    }

    /// Constrain x < y (for Ord-comparable values).
    pub fn add_less_than(&mut self, x: VarId, y: VarId)
    where
        D: 'static,
        D::Value: PartialOrd + Send + Sync,
    {
        self.add_constraint(constraint::LambdaConstraint::new(
            vec![x, y],
            move |assignment| match (&assignment[x as usize], &assignment[y as usize]) {
                (Some(a), Some(b)) => a < b,
                _ => true,
            },
            format!("less_than({x},{y})"),
        ));
    }

    /// Constrain x > y (for Ord-comparable values).
    pub fn add_greater_than(&mut self, x: VarId, y: VarId)
    where
        D: 'static,
        D::Value: PartialOrd + Send + Sync,
    {
        self.add_constraint(constraint::LambdaConstraint::new(
            vec![x, y],
            move |assignment| match (&assignment[x as usize], &assignment[y as usize]) {
                (Some(a), Some(b)) => a > b,
                _ => true,
            },
            format!("greater_than({x},{y})"),
        ));
    }

    /// Build the adjacency graph. Must be called after all variables and
    /// constraints have been added, before calling `solve()`.
    pub fn finalize(&mut self)
    where
        D::Value: PartialEq + 'static,
    {
        let num_vars = self.variables.len();
        self.adjacency = Some(Adjacency::build(num_vars, &self.constraints));

        self.constraint_weights = vec![1.0; self.constraints.len()];
        self.var_constraint_ids = vec![Vec::new(); num_vars];
        for (ci, c) in self.constraints.iter().enumerate() {
            for &v in c.scope() {
                self.var_constraint_ids[v as usize].push(ci);
            }
        }
    }
}
