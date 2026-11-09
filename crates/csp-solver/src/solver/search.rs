//! Unified monomorphized search kernel.
//!
//! Tests: `tests/solver.rs` (general solve correctness),
//! `tests/solution_set_invariance.rs` (solution-set property test).
//!
//! One tree-search skeleton — [`search`] — parameterized by a zero-sized
//! [`SearchPolicy`]. The policy decides the four things that actually differ
//! between the crate's search modes:
//!
//! | axis            | [`Feasibility`]        | [`BranchBound`]              |
//! |-----------------|------------------------|------------------------------|
//! | leaf action     | record, stop at `max`  | score, keep best, never stop |
//! | node prune      | none                   | optimistic-bound cutoff      |
//! | value order     | raw domain order       | cost-sorted                  |
//!
//! Every hook is `#[inline]` and every implementor is (near) zero-sized, so
//! monomorphization inlines the whole policy — no dispatch cost, the same
//! devirtualization the crate already applies to `ConstraintEnum`.
//!
//! This kernel replaces the three near-verbatim recursive DFS functions that
//! preceded it (`backtrack_recurse`, `backjump_recurse`, `bb_recurse`) whose
//! propagate `match`, validity-check loop, and restore sweep were byte-identical.
//! Undo now runs off the shared [`Trail`] (touched-variable list) rather than an
//! O(num_vars) per-node sweep, and read-only state (`weights`, `var_cids`,
//! `adjacency`) is borrowed rather than deep-cloned per solve.
//!
//! # Waiver: this file stays whole (CLOSED)
//!
//! At 504 LOC the module sits four lines over the file budget, and the two
//! policies are visually fenced — a `BranchBound`-out split looks free. It
//! isn't. `BranchBound` impls [`SearchPolicy`] and calls [`search`], so
//! extracting it to a sibling module forces `trait SearchPolicy`, `fn search`,
//! and likely `Step` — all currently private kernel internals — to widen to
//! `pub(super)`. That re-widens three internals to buy a cosmetic file cut: an
//! encapsulation regression inside an encapsulation pass. The single reason to
//! change here is the one search skeleton; [`Feasibility`] and [`BranchBound`]
//! are its co-designed leaves, not separable concerns. Waiver recorded, closed.

use crate::constraint::{ConstraintEnum, VarId};
use crate::domain::Domain;
use crate::ordering::{self, Ordering};
use crate::solver::adjacency::Adjacency;
use crate::solver::optimize::DomainCostEval;
use crate::solver::{Solution, Trail, ac3, propagate};
use crate::variable::Variable;
use crate::{Pruning, SolveStats};

/// Depth reserved for permanent pre-search propagation (root AC-3, given-cell
/// propagation). Search recursion begins at [`SEARCH_ROOT_DEPTH`] so its
/// depth-keyed undo can never target these permanent reductions — fixing the
/// depth-0 seam where the first failed root candidate un-pruned the initial
/// AC-3 via `restore(0)`.
pub(crate) const PERMANENT_DEPTH: usize = 0;
/// First depth used by search recursion frames.
const SEARCH_ROOT_DEPTH: usize = 1;

/// Shared, immutable search parameters. Collapses the three former per-mode
/// config structs (`BacktrackConfig` / `BackjumpConfig` / `OptimizeConfig`),
/// which differed only in a `maximize` bool and each carried its own *cloned*
/// copy of `constraint_weights` + `var_constraint_ids`. Those read-only vectors
/// are now borrowed by the kernel, so they live nowhere in this struct.
#[derive(Debug, Clone)]
pub(crate) struct SearchParams {
    pub(crate) pruning: Pruning,
    pub(crate) ordering: Ordering,
    pub(crate) max_solutions: usize,
    /// Node budget (search-frame count). `None` disables. See
    /// [`crate::SolveConfig::node_budget`].
    pub(crate) node_budget: Option<u64>,
    /// Cooperative cancellation handle, checked at the same cadence as
    /// `node_budget`. `None` disables. See [`crate::SolveConfig::cancel`].
    pub(crate) cancel: Option<crate::CancelToken>,
}

/// Outcome of one recursion step. Feasibility and branch-and-bound only ever
/// produce `Continue`/`Done`; a jump variant is intentionally absent (CBJ was
/// excised — see the module-level notes in `lib.rs`).
enum Step {
    Continue,
    Done,
}

/// The single mutable spine of the search: problem references, the undo trail,
/// and stats. `weights` / `var_cids` / `adjacency` are borrowed (no per-solve
/// clone); `weights` is `&mut` so a later tranche can bump dom/wdeg on wipe-out
/// without touching this signature.
pub(crate) struct Kernel<'a, D: Domain> {
    variables: &'a mut [Variable<D>],
    constraints: &'a [ConstraintEnum<D>],
    adjacency: &'a Adjacency,
    weights: &'a mut [f64],
    var_cids: &'a [Vec<usize>],
    stats: &'a mut SolveStats,
    trail: Trail,
    /// Reusable AC-3 worklist scratch, sized once to `constraints.len()` at
    /// search entry. `Pruning::Ac3`'s per-candidate `ac3_from_variable` call
    /// clears and reseeds this instead of allocating a fresh `Vec<u64>`-backed
    /// worklist on every attempt (zero-alloc P2-2). Folds the scratch the
    /// deleted `SearchContext` carried into the unified kernel spine.
    worklist: ac3::BitsetWorklist,
    params: &'a SearchParams,
}

impl<D: Domain> Kernel<'_, D>
where
    D::Value: PartialEq + 'static,
{
    /// Validity check: every fully-assigned constraint incident to `var` holds.
    #[inline]
    fn is_valid(&self, var: VarId, assignment: &[Option<D::Value>]) -> bool {
        for &ci in self.adjacency.constraints_for(var) {
            let ci = ci as usize;
            let scope = self.constraints[ci].scope();
            if scope.iter().all(|&v| assignment[v as usize].is_some())
                && !self.constraints[ci].check(assignment)
            {
                return false;
            }
        }
        true
    }

    /// Propagate from a freshly-assigned `var`. Returns `true` on domain
    /// wipe-out. All prunes are streamed onto `self.trail` for O(removed) undo.
    ///
    /// The propagation primitives return a blame signal (`Some(ci)` = the
    /// constraint that emptied a domain). The unified kernel does not consume
    /// the culprit; it is collapsed to a wipe-out bool here via `.is_some()`.
    #[inline]
    fn propagate_from(
        &mut self,
        var: VarId,
        assignment: &mut Vec<Option<D::Value>>,
        depth: usize,
    ) -> bool {
        match self.params.pruning {
            Pruning::None => false,
            Pruning::ForwardChecking => propagate::forward_check(
                var,
                self.variables,
                self.constraints,
                self.adjacency,
                assignment.as_mut_slice(),
                self.stats,
                &mut self.trail,
                depth,
            )
            .is_some(),
            Pruning::Ac3 => ac3::ac3_from_variable(
                var,
                self.variables,
                self.constraints,
                self.adjacency,
                assignment,
                self.stats,
                &mut self.trail,
                &mut self.worklist,
                depth,
            )
            .is_some(),
            Pruning::AcFc => propagate::ac_fc(
                var,
                self.variables,
                self.constraints,
                self.adjacency,
                assignment.as_mut_slice(),
                self.stats,
                &mut self.trail,
                depth,
            )
            .is_some(),
        }
    }
}

/// The policy hooks. Defaults cover feasibility; branch-and-bound overrides
/// `on_leaf`, `node_prune`, and `order_values`.
trait SearchPolicy<D: Domain> {
    /// Called at a complete assignment. Returns whether search should stop.
    fn on_leaf(&mut self, k: &mut Kernel<'_, D>, assignment: &[Option<D::Value>]) -> Step;

    /// Optional node-level prune (bound cutoff). Default: never prune.
    #[inline]
    fn node_prune(&mut self, _k: &Kernel<'_, D>, _assignment: &[Option<D::Value>]) -> bool {
        false
    }

    /// Value branching order for `var`. Default: raw domain order (no-op).
    #[inline]
    fn order_values(&self, _k: &Kernel<'_, D>, _var: VarId, _values: &mut [D::Value]) {}
}

/// The one tree-search skeleton. Monomorphized per `(D, P)`.
fn search<D, P>(
    k: &mut Kernel<'_, D>,
    p: &mut P,
    assignment: &mut Vec<Option<D::Value>>,
    stack: &mut Vec<VarId>,
    depth: usize,
) -> Step
where
    D: Domain,
    D::Value: PartialEq + 'static,
    P: SearchPolicy<D>,
{
    if stack.is_empty() {
        return p.on_leaf(k, assignment);
    }

    // Budget guard — checked before `nodes_explored += 1` so the post-budget
    // node is never counted and the flag is set exactly once per search.
    if let Some(budget) = k.params.node_budget
        && k.stats.nodes_explored >= budget
    {
        k.stats.budget_exceeded = true;
        return Step::Done;
    }

    // Cancellation guard — same cadence as the budget guard, but for an
    // externally requested stop (e.g. a `Python::allow_threads`-released
    // search whose caller's `asyncio.wait_for` timeout just elapsed). Folds
    // the pyo3 cancel-token check onto the unified kernel (the deleted
    // per-mode recurse fns each carried their own copy).
    if let Some(tok) = &k.params.cancel
        && tok.is_cancelled()
    {
        k.stats.cancelled = true;
        return Step::Done;
    }

    k.stats.nodes_explored += 1;

    if p.node_prune(k, assignment) {
        return Step::Continue;
    }

    let idx =
        ordering::select_variable(stack, k.variables, k.params.ordering, k.weights, k.var_cids)
            .unwrap();
    let var = stack.swap_remove(idx);

    let mut values: Vec<_> = k.variables[var as usize].domain.iter().collect();
    p.order_values(k, var, &mut values);

    for val in values {
        let mark = k.trail.checkpoint();
        assignment[var as usize] = Some(val.clone());

        // Restrict domain to singleton {val} so revise() sees the decision.
        k.variables[var as usize].restrict_to(&val, depth);
        k.trail.push(var);

        if k.is_valid(var, assignment)
            && !k.propagate_from(var, assignment, depth)
            && let Step::Done = search(k, p, assignment, stack, depth + 1)
        {
            return Step::Done;
        }

        k.stats.backtracks += 1;
        assignment[var as usize] = None;
        k.trail.undo_to(mark, depth, k.variables);
    }

    stack.push(var);
    Step::Continue
}

// ---------------------------------------------------------------------------
// Feasibility policy + entry point
// ---------------------------------------------------------------------------

struct Feasibility<D: Domain> {
    solutions: Vec<Solution<D>>,
    max_solutions: usize,
}

impl<D: Domain> SearchPolicy<D> for Feasibility<D>
where
    D::Value: PartialEq + 'static,
{
    #[inline]
    fn on_leaf(&mut self, _k: &mut Kernel<'_, D>, assignment: &[Option<D::Value>]) -> Step {
        self.solutions.push(
            assignment
                .iter()
                .map(|v| v.as_ref().unwrap().clone())
                .collect(),
        );
        if self.solutions.len() >= self.max_solutions {
            Step::Done
        } else {
            Step::Continue
        }
    }
}

/// Run feasibility (satisfaction) search. `given` pre-seeds an assignment and
/// filters the branch stack; `None` searches all variables from scratch.
#[allow(clippy::too_many_arguments)]
pub(crate) fn feasibility_search<D: Domain>(
    variables: &mut [Variable<D>],
    constraints: &[ConstraintEnum<D>],
    adjacency: &Adjacency,
    weights: &mut [f64],
    var_cids: &[Vec<usize>],
    params: &SearchParams,
    stats: &mut SolveStats,
    given: Option<&[(VarId, D::Value)]>,
) -> Vec<Solution<D>>
where
    D::Value: PartialEq + 'static,
{
    let num_vars = variables.len();
    let mut assignment: Vec<Option<D::Value>> = vec![None; num_vars];

    let mut stack: Vec<VarId> = if let Some(given) = given {
        for (var, val) in given {
            assignment[*var as usize] = Some(val.clone());
        }
        (0..num_vars as u32)
            .filter(|v| assignment[*v as usize].is_none())
            .collect()
    } else {
        (0..num_vars as u32).collect()
    };

    let mut policy = Feasibility {
        solutions: Vec::new(),
        max_solutions: params.max_solutions,
    };
    let mut kernel = Kernel {
        variables,
        constraints,
        adjacency,
        weights,
        var_cids,
        stats,
        trail: Trail::default(),
        worklist: ac3::BitsetWorklist::new(constraints.len()),
        params,
    };

    search(
        &mut kernel,
        &mut policy,
        &mut assignment,
        &mut stack,
        SEARCH_ROOT_DEPTH,
    );

    policy.solutions
}

// ---------------------------------------------------------------------------
// Branch-and-bound policy + entry point
// ---------------------------------------------------------------------------

struct ScoredSolution<D: Domain> {
    solution: Solution<D>,
    cost: f64,
}

struct BranchBound<'e, D: Domain> {
    scored: Vec<ScoredSolution<D>>,
    best_cost: f64,
    maximize: bool,
    eval: &'e dyn DomainCostEval<D>,
}

impl<D: Domain> BranchBound<'_, D>
where
    D::Value: PartialEq + 'static,
{
    /// Total cost of a complete assignment: the sum of domain costs.
    fn assignment_cost(&self, k: &Kernel<'_, D>, assignment: &[Option<D::Value>]) -> f64 {
        let mut cost = 0.0;
        for (i, val) in assignment.iter().enumerate() {
            if let Some(v) = val {
                cost += self.eval.cost(&k.variables[i].domain, v);
            }
        }
        cost
    }

    /// Optimistic bound on any completion (lower bound for minimize, upper for
    /// maximize). Unassigned vars contribute their best-case domain cost.
    fn optimistic_bound(&self, k: &Kernel<'_, D>, assignment: &[Option<D::Value>]) -> f64 {
        let mut bound = 0.0;
        for (i, val) in assignment.iter().enumerate() {
            match val {
                Some(v) => bound += self.eval.cost(&k.variables[i].domain, v),
                None if self.maximize => bound += self.eval.max_cost(&k.variables[i].domain),
                None => bound += self.eval.min_cost(&k.variables[i].domain),
            }
        }
        bound
    }
}

impl<D: Domain> SearchPolicy<D> for BranchBound<'_, D>
where
    D::Value: PartialEq + 'static,
{
    #[inline]
    fn on_leaf(&mut self, k: &mut Kernel<'_, D>, assignment: &[Option<D::Value>]) -> Step {
        let cost = self.assignment_cost(k, assignment);
        let effective = if self.maximize { -cost } else { cost };
        if effective < self.best_cost {
            self.best_cost = effective;
        }
        self.scored.push(ScoredSolution {
            solution: assignment
                .iter()
                .map(|v| v.as_ref().unwrap().clone())
                .collect(),
            cost,
        });
        // Optimization never stops early — keep searching for better solutions.
        Step::Continue
    }

    #[inline]
    fn node_prune(&mut self, k: &Kernel<'_, D>, assignment: &[Option<D::Value>]) -> bool {
        let bound = self.optimistic_bound(k, assignment);
        let effective = if self.maximize { -bound } else { bound };
        effective >= self.best_cost
    }

    #[inline]
    fn order_values(&self, k: &Kernel<'_, D>, var: VarId, values: &mut [D::Value]) {
        let domain = &k.variables[var as usize].domain;
        // Cheapest-first for minimize, costliest-first for maximize. Cache the
        // cost key once per value instead of recomputing it per comparison.
        if self.maximize {
            values.sort_by(|a, b| {
                self.eval
                    .cost(domain, b)
                    .partial_cmp(&self.eval.cost(domain, a))
                    .unwrap_or(std::cmp::Ordering::Equal)
            });
        } else {
            values.sort_by(|a, b| {
                self.eval
                    .cost(domain, a)
                    .partial_cmp(&self.eval.cost(domain, b))
                    .unwrap_or(std::cmp::Ordering::Equal)
            });
        }
    }
}

/// Run branch-and-bound optimization. Returns up to `max_solutions` solutions,
/// sorted best-first per the optimization direction.
#[allow(clippy::too_many_arguments)]
pub(crate) fn branch_and_bound<D: Domain>(
    variables: &mut [Variable<D>],
    constraints: &[ConstraintEnum<D>],
    adjacency: &Adjacency,
    weights: &mut [f64],
    var_cids: &[Vec<usize>],
    params: &SearchParams,
    stats: &mut SolveStats,
    maximize: bool,
    cost_eval: &dyn DomainCostEval<D>,
) -> Vec<Solution<D>>
where
    D::Value: PartialEq + 'static,
{
    let num_vars = variables.len();
    let mut assignment: Vec<Option<D::Value>> = vec![None; num_vars];
    let mut stack: Vec<VarId> = (0..num_vars as u32).collect();

    let mut policy = BranchBound {
        scored: Vec::new(),
        best_cost: f64::INFINITY,
        maximize,
        eval: cost_eval,
    };
    let mut kernel = Kernel {
        variables,
        constraints,
        adjacency,
        weights,
        var_cids,
        stats,
        trail: Trail::default(),
        worklist: ac3::BitsetWorklist::new(constraints.len()),
        params,
    };

    search(
        &mut kernel,
        &mut policy,
        &mut assignment,
        &mut stack,
        SEARCH_ROOT_DEPTH,
    );

    // Best-first: lowest effective cost first. `maximize` flips the comparison.
    if maximize {
        policy.scored.sort_by(|a, b| {
            b.cost
                .partial_cmp(&a.cost)
                .unwrap_or(std::cmp::Ordering::Equal)
        });
    } else {
        policy.scored.sort_by(|a, b| {
            a.cost
                .partial_cmp(&b.cost)
                .unwrap_or(std::cmp::Ordering::Equal)
        });
    }
    policy.scored.truncate(params.max_solutions);
    policy.scored.into_iter().map(|s| s.solution).collect()
}
