//! Core constraint trait and supporting types.

use std::fmt::Debug;

use crate::domain::Domain;
use crate::variable::Variable;

/// Unique variable identifier (index into the variable array).
pub type VarId = u32;

/// Result of running `revise` on a constraint.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Revision {
    Unchanged,
    Changed,
    Unsatisfiable,
}

/// Thread-safety marker for `Constraint`, scoped to the `py` feature.
///
/// The `pyo3::Python::allow_threads` boundary — compiled **only** under the
/// `py` feature — releases the GIL and runs the solver off-thread, which
/// requires the whole `Csp<D>` (and therefore every `Box<dyn Constraint<D>>`
/// it holds) to be `Send`. That requirement is real *only* at the py boundary.
///
/// Making `Send + Sync` an unconditional supertrait of `Constraint` (the prior
/// shape) rejected `!Send`/`!Sync` external implementors — concretely
/// bbnf-lang's `RefConstraint`, which holds an `Rc<RefCell<…>>` obligation
/// sink (E0277). The old "non-breaking, purely additive" claim was therefore
/// false: it broke a real downstream consumer.
///
/// This marker carries `Send + Sync` **under `py`** and is **vacuous
/// otherwise**, so:
///   * `--features py`  → `dyn Constraint<D>: Send + Sync` transitively via the
///     supertrait chain, hence `Csp<D>: Send` and `allow_threads` compiles; a
///     `!Send` constraint is rejected loudly at its `impl` site (fail-explicit,
///     never a silent downgrade).
///   * default build (bbnf's) → the bound is inert; `RefConstraint` and every
///     other lattice/reference constraint compiles unchanged, zero bbnf edits.
///
/// The two configurations are kept honest by the sync gate, which compiles
/// BOTH (`--features py` and the default). See `scripts/sync-csp-solver-vendor`.
#[cfg(feature = "py")]
pub trait ThreadSafe: Send + Sync {}
#[cfg(feature = "py")]
impl<T: Send + Sync> ThreadSafe for T {}
#[cfg(not(feature = "py"))]
pub trait ThreadSafe {}
#[cfg(not(feature = "py"))]
impl<T> ThreadSafe for T {}

/// A constraint over one or more CSP variables.
pub trait Constraint<D: Domain>: Debug + ThreadSafe {
    /// The variables this constraint involves (its "scope").
    fn scope(&self) -> &[VarId];

    /// Check whether a full or partial assignment satisfies this constraint.
    fn check(&self, assignment: &[Option<D::Value>]) -> bool;

    /// AC-3 style revision: prune unsupported values from domains.
    ///
    /// Dispatches on scope size to the module-private `revise_unary_default`
    /// / `revise_binary_default` helpers below (n-ary constraints get their
    /// own devirtualized `revise_impl` — `AllDifferent`,
    /// `AllDifferentExcept` — the default here only needs to cover what
    /// `LambdaConstraint`/`Custom` actually construct: `add_equals` is
    /// unary, `add_less_than`/`add_greater_than` are binary).
    ///
    /// Previously scope.len() == 1 unconditionally returned `Unchanged` —
    /// silently disabling propagation for every unary constraint
    /// (`add_equals`), which was then enforced only by `check()` at
    /// assignment time (Pass-1 propagation audit, P2-1). Fixed below.
    fn revise(&self, vars: &mut [Variable<D>], depth: usize) -> Revision {
        match self.scope().len() {
            1 => revise_unary_default(self, vars, depth),
            2 => revise_binary_default(self, vars, depth),
            _ => Revision::Unchanged,
        }
    }
}

/// Default unary revision: prune every domain value unsupported by
/// `check()` in isolation. Iterates the domain's owned snapshot directly
/// (`Domain::iter`'s `+ use<Self>` bound), no heap `Vec`. A free function
/// (not a trait method) so it doesn't grow `Constraint`'s public surface —
/// it's purely `revise`'s own dispatch target.
///
/// `changed` folds in [`Variable::prune`]'s bool: a pruned value that was
/// already absent (or that the domain refuses to remove) does not count as
/// a revision. See `revise_binary_default` for why this honesty is
/// load-bearing rather than cosmetic.
fn revise_unary_default<D: Domain, C: Constraint<D> + ?Sized>(
    c: &C,
    vars: &mut [Variable<D>],
    depth: usize,
) -> Revision {
    let xi = c.scope()[0] as usize;
    let mut assignment: Vec<Option<D::Value>> = vec![None; vars.len()];
    let mut changed = false;

    for vi in vars[xi].domain.iter() {
        assignment[xi] = Some(vi.clone());
        if !c.check(&assignment) {
            changed |= vars[xi].prune(&vi, depth);
        }
    }

    if vars[xi].domain.is_empty() {
        return Revision::Unsatisfiable;
    }

    if changed {
        Revision::Changed
    } else {
        Revision::Unchanged
    }
}

/// Default binary revision: the "change-mask" rewrite of the previous
/// 5-`Vec` implementation (one `vars.len()`-sized `assignment`, plus four
/// `domain.values()` snapshots — `vals_i`/`vals_j` each collected twice,
/// once per direction pass, per the Pass-1 constraint audit's F5).
///
/// The `assignment` scratch is kept (its size is dictated by
/// `Constraint::check`'s existing `&[Option<D::Value>]` contract, indexed
/// by absolute `VarId` — narrowing it would be a breaking signature change
/// reaching every hand-rolled `revise`/`check` in bbnf-lang's 11+ `Custom`
/// constraint types). The four domain snapshots are eliminated: each pass
/// calls `domain.iter()` fresh inside the loop instead of pre-collecting
/// into a `Vec` — for `BitsetDomain` (the crate's only production domain)
/// re-deriving the iterator is a cheap `u128` copy, not a re-scan, so this
/// costs nothing extra while dropping 4 allocations to 0. The second
/// pass's `vals_i` must still reflect pass 1's pruning (correctness-
/// critical: pass 1 may have narrowed `xi`), which a live
/// `vars[xi].domain.iter()` gives for free — no explicit re-collection
/// needed.
///
/// # Reporting `Changed` honestly (the lattice-hang repair)
///
/// `changed` folds in [`Variable::prune`]'s bool return rather than being
/// set unconditionally in the unsupported branch. On a finite domain this
/// is behavior-neutral — the enumerate loop only ever prunes a value the
/// domain currently holds, so `prune` always removes it and returns
/// `true`. On a **lattice** domain it is the difference between converging
/// and hanging: `BitsetLatticeDomain::remove` is a no-op (lattice values
/// only grow via `join`, never shrink), so every `prune` returns `false`
/// and this arc-consistency revise legitimately changes nothing. The old
/// unconditional `changed = true` reported `Revision::Changed` while
/// pruning nothing — a lie that made AC-3 (and the monotonic sweep)
/// re-enqueue the same arcs forever on a disjoint-seed lattice chain (W1
/// bench-attribution F1). Honoring the bool lets the worklist drain.
/// A check-based constraint cannot *grow* a lattice domain regardless;
/// genuine lattice propagators supply their own join-based `revise`.
fn revise_binary_default<D: Domain, C: Constraint<D> + ?Sized>(
    c: &C,
    vars: &mut [Variable<D>],
    depth: usize,
) -> Revision {
    let scope = c.scope();
    let xi = scope[0] as usize;
    let xj = scope[1] as usize;
    let mut changed = false;

    let mut assignment: Vec<Option<D::Value>> = vec![None; vars.len()];

    for vi in vars[xi].domain.iter() {
        let mut supported = false;
        assignment[xi] = Some(vi.clone());
        for vj in vars[xj].domain.iter() {
            assignment[xj] = Some(vj);
            if c.check(&assignment) {
                supported = true;
                break;
            }
        }
        if !supported {
            changed |= vars[xi].prune(&vi, depth);
        }
    }
    assignment[xi] = None;
    assignment[xj] = None;

    if vars[xi].domain.is_empty() {
        return Revision::Unsatisfiable;
    }

    for vj in vars[xj].domain.iter() {
        let mut supported = false;
        assignment[xj] = Some(vj.clone());
        for vi in vars[xi].domain.iter() {
            assignment[xi] = Some(vi);
            if c.check(&assignment) {
                supported = true;
                break;
            }
        }
        if !supported {
            changed |= vars[xj].prune(&vj, depth);
        }
    }

    if vars[xj].domain.is_empty() {
        return Revision::Unsatisfiable;
    }

    if changed {
        Revision::Changed
    } else {
        Revision::Unchanged
    }
}
