//! N-ary all-different constraint.
//!
//! Tests: `tests/solver.rs` (live path via `Csp::add_all_different`);
//! `tests/sudoku.rs`, `tests/futoshiki.rs` exercise the GAC core end-to-end.

use crate::domain::Domain;
use crate::variable::Variable;

use super::traits::{Constraint, Revision, VarId};

#[derive(Debug)]
pub struct AllDifferent {
    pub(crate) scope: Vec<VarId>,
    /// Stable per-constraint id keying the GAC matching warm-start cache.
    gac_id: u32,
}

impl AllDifferent {
    pub fn new(vars: Vec<VarId>) -> Self {
        Self {
            scope: vars,
            gac_id: crate::solver::gac::next_gac_id(),
        }
    }

    pub(crate) fn check_impl<V: PartialEq>(&self, assignment: &[Option<V>]) -> bool {
        let assigned: Vec<&V> = self
            .scope
            .iter()
            .filter_map(|&v| assignment[v as usize].as_ref())
            .collect();
        for i in 0..assigned.len() {
            for j in (i + 1)..assigned.len() {
                if assigned[i] == assigned[j] {
                    return false;
                }
            }
        }
        true
    }

    /// Singleton removal followed by GAC when enough participants remain live.
    ///
    /// Singleton removal always runs (it is the only step that detects two
    /// *assigned* variables sharing a value — GAC skips assigned vars). Above a
    /// dynamic live-unassigned-count gate, Régin GAC then adds the Hall-set
    /// pruning arc consistency misses; below it, GAC would find nothing more, so
    /// it is skipped. The gate is on the *live* count, not the constraint's
    /// original arity, so deep search nodes with few free variables pay nothing.
    pub(crate) fn revise_impl<D: Domain>(&self, vars: &mut [Variable<D>], depth: usize) -> Revision
    where
        D::Value: PartialEq + 'static,
    {
        let mut changed = false;
        let singletons: Vec<(VarId, D::Value)> = self
            .scope
            .iter()
            .filter_map(|&v| {
                vars[v as usize]
                    .domain
                    .singleton_value()
                    .map(|val| (v, val))
            })
            .collect();

        for (sv, sval) in &singletons {
            for &other in &self.scope {
                if other == *sv {
                    continue;
                }
                if vars[other as usize].prune(sval, depth) {
                    changed = true;
                }
                if vars[other as usize].domain.is_empty() {
                    return Revision::Unsatisfiable;
                }
            }
        }

        // Dynamic gate: count live (non-singleton, non-empty) variables.
        let live = self
            .scope
            .iter()
            .filter(|&&v| {
                let d = &vars[v as usize].domain;
                !d.is_singleton() && !d.is_empty()
            })
            .count();

        if live >= crate::solver::gac::GAC_MIN_PARTICIPANTS
            && crate::solver::gac::GAC_IN_ALLDIFF_ENABLED.load(std::sync::atomic::Ordering::Relaxed)
        {
            match crate::solver::gac::propagate_gac_core(
                &self.scope,
                None,
                vars,
                depth,
                Some(self.gac_id),
            ) {
                Revision::Unsatisfiable => return Revision::Unsatisfiable,
                Revision::Changed => changed = true,
                Revision::Unchanged => {}
            }
        }

        if changed {
            Revision::Changed
        } else {
            Revision::Unchanged
        }
    }
}

impl<D: Domain> Constraint<D> for AllDifferent
where
    D::Value: PartialEq + 'static,
{
    fn scope(&self) -> &[VarId] {
        &self.scope
    }
    fn check(&self, assignment: &[Option<D::Value>]) -> bool {
        self.check_impl(assignment)
    }
    fn revise(&self, vars: &mut [Variable<D>], depth: usize) -> Revision {
        self.revise_impl(vars, depth)
    }
}
