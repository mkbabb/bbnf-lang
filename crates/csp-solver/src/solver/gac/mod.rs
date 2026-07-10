//! Unified GAC all-different core (Régin 1994), sentinel-generic and incremental.
//!
//! One propagator body serves both the plain all-different constraint and the
//! sentinel-aware "all-different-except" variant. The distinction is a single
//! [`Option<&D::Value>`] parameter:
//!
//! * `sentinel = None` — plain all-different. Every unassigned variable must be
//!   matched to a distinct value.
//! * `sentinel = Some(s)` — the value `s` is an "escape valve" any number of
//!   variables may take; it is removed from the value side of the bipartite
//!   graph, and variables whose domain has narrowed to `{s}` drop out of the
//!   variable side.
//!
//! The Régin pipeline (Hopcroft-Karp maximum matching → residual graph → Tarjan
//! SCC → prune edges outside every maximum matching) is identical for both; only
//! participant/value collection and free-vertex reachability seeding branch on
//! the sentinel. The graph primitives live in [`matching`].
//!
//! # Incrementality
//!
//! Two costs the from-scratch predecessor paid on *every* `revise()` are gone:
//!
//! 1. **Per-call heap allocation.** All working buffers (adjacency, matching,
//!    residual graph, Tarjan stacks) live in a thread-local [`GacScratch`] that
//!    is cleared — not freed — between calls. The value universe is the sole
//!    per-call `Vec<V>` the generic bound cannot pool.
//! 2. **Cold Hopcroft-Karp.** With a stable constraint id, the previous maximum
//!    matching is cached per constraint and warm-starts the next call: cached
//!    edges still valid against the live domains seed `match_u`/`match_v`, and
//!    Hopcroft-Karp only augments from the vertices left free — O(E) repair
//!    rather than O(E·√V) reconstruction. The cache is a pure hint: every seeded
//!    edge is validated against live domains, so a stale cache costs extra
//!    augmentation, never correctness.

mod matching;

use std::any::Any;
use std::cell::RefCell;
use std::collections::HashMap;
use std::sync::atomic::{AtomicBool, AtomicU32, AtomicU64, Ordering};

use crate::constraint::traits::{Revision, VarId};
use crate::domain::Domain;
use crate::variable::Variable;

use matching::{NONE, hopcroft_karp, reset_adj, tarjan_scc};

/// Instrumentation: total entries into the unified GAC core (all variants).
pub static GAC_CORE_CALLS: AtomicU64 = AtomicU64::new(0);

/// Monotonic source of per-constraint cache ids.
static NEXT_GAC_ID: AtomicU32 = AtomicU32::new(0);

/// Allocate a fresh, process-unique constraint id for matching-cache keying.
pub fn next_gac_id() -> u32 {
    NEXT_GAC_ID.fetch_add(1, Ordering::Relaxed)
}

/// Below this live-participant count, GAC finds nothing singleton removal
/// misses, so the caller's cheaper path is preferred. Exposed so constraints
/// gate consistently.
pub const GAC_MIN_PARTICIPANTS: usize = 3;

/// A/B measurement toggle for the plain `AllDifferent` GAC path (Sudoku,
/// Futoshiki). Default on; flipping it off reverts `AllDifferent` to
/// singleton-removal-only so the pruning-strength delta can be measured.
pub static GAC_IN_ALLDIFF_ENABLED: AtomicBool = AtomicBool::new(true);

/// Cap on the per-thread matching cache; cleared wholesale past this many
/// distinct constraints so a long-lived worker thread cannot leak unboundedly.
const CACHE_CAP: usize = 8192;

// ---------------------------------------------------------------------------
// Reusable scratch
// ---------------------------------------------------------------------------

/// Per-thread, per-value-type reusable working set for the GAC core.
///
/// Generic in the value type only for the value universe and matching cache;
/// every other buffer is integer-indexed and value-agnostic.
struct GacScratch<V> {
    participants: Vec<usize>,
    has_sentinel: Vec<bool>,
    assigned_ns: Vec<V>,
    all_vals: Vec<V>,
    adj: Vec<Vec<u32>>,
    match_u: Vec<u32>,
    match_v: Vec<u32>,
    dist: Vec<u32>,
    queue: Vec<u32>,
    res_adj: Vec<Vec<u32>>,
    reachable: Vec<bool>,
    bfs: Vec<u32>,
    t_index: Vec<u32>,
    t_lowlink: Vec<u32>,
    t_onstack: Vec<bool>,
    t_scc: Vec<u32>,
    t_stack: Vec<u32>,
    t_call: Vec<(u32, u32)>,
    cache: HashMap<u32, Vec<Option<V>>>,
    /// Generation-stamped reverse map for the integer fast path (Beat 1).
    ///
    /// When `D::Value` is a small non-negative integer, `val_index[k]` holds
    /// the `all_vals` slot for value `k` **iff** `val_index_gen[k] == cur_gen`.
    /// The stamp makes the per-call reset O(1) (bump `cur_gen`) so the buffer
    /// never leaks a prior call's numbering across a value-universe shrink —
    /// see [`INV-A`](self). Non-integer / out-of-range values fall back to the
    /// generic `PartialEq` `position` scan, so no `Hash`/`Ord`/`ValueIndex`
    /// bound is introduced.
    val_index: Vec<u32>,
    val_index_gen: Vec<u32>,
    cur_gen: u32,
}

/// Upper bound on the integer key the value→index fast path will address.
/// A value above this (or negative, or non-integer) uses the `position`-scan
/// fallback, bounding `val_index`'s worst-case footprint. Live GAC value
/// universes are tiny (`BitsetDomain` is 0..128; assignment columns 0..n_cols),
/// so the cap is never approached in practice.
const MAX_FAST_INDEX: usize = 1 << 20;

/// If `v` is one of the supported integer types and lands in
/// `0..=MAX_FAST_INDEX`, return it as a reverse-map key; otherwise `None`
/// (caller falls back to the `PartialEq` scan). Uses the `Any` blanket impl
/// available to every `'static` type, so it adds **no** trait bound to the
/// generic value — `FiniteDomain<String>` still routes entirely through the
/// scan and compiles unchanged.
fn fast_index<V: 'static>(v: &V) -> Option<usize> {
    let any = v as &dyn Any;
    macro_rules! try_ints {
        ($($t:ty),*) => {$(
            if let Some(&x) = any.downcast_ref::<$t>() {
                return usize::try_from(x).ok().filter(|&k| k <= MAX_FAST_INDEX);
            }
        )*};
    }
    try_ints!(u8, u16, u32, u64, usize, i8, i16, i32, i64, isize);
    None
}

impl<V> Default for GacScratch<V> {
    fn default() -> Self {
        Self {
            participants: Vec::new(),
            has_sentinel: Vec::new(),
            assigned_ns: Vec::new(),
            all_vals: Vec::new(),
            adj: Vec::new(),
            match_u: Vec::new(),
            match_v: Vec::new(),
            dist: Vec::new(),
            queue: Vec::new(),
            res_adj: Vec::new(),
            reachable: Vec::new(),
            bfs: Vec::new(),
            t_index: Vec::new(),
            t_lowlink: Vec::new(),
            t_onstack: Vec::new(),
            t_scc: Vec::new(),
            t_stack: Vec::new(),
            t_call: Vec::new(),
            cache: HashMap::new(),
            val_index: Vec::new(),
            val_index_gen: Vec::new(),
            cur_gen: 0,
        }
    }
}

thread_local! {
    /// One type-erased [`GacScratch`] slot per thread. The concrete `V` for a
    /// given solve is fixed, so the slot re-materializes only if a thread runs
    /// solves over different value types.
    static SCRATCH: RefCell<Box<dyn Any>> = RefCell::new(Box::new(()));
}

fn with_scratch<V, R>(f: impl FnOnce(&mut GacScratch<V>) -> R) -> R
where
    V: 'static,
{
    SCRATCH.with(|slot| {
        let mut b = slot.borrow_mut();
        if !b.is::<GacScratch<V>>() {
            *b = Box::new(GacScratch::<V>::default());
        }
        let s = b.downcast_mut::<GacScratch<V>>().unwrap();
        f(s)
    })
}

// ---------------------------------------------------------------------------
// Unified GAC core
// ---------------------------------------------------------------------------

/// Run Régin GAC on `scope`, sentinel-generic and incremental.
///
/// * `sentinel = None` → plain all-different; `Some(s)` → all-different-except.
/// * `gac_id = Some(id)` enables the per-constraint matching warm-start cache;
///   `None` runs stateless (used by the one-shot public wrappers and tests).
///
/// Only requires `D::Value: Clone + PartialEq + Debug` (all implied by `Domain`)
/// plus `'static` for the thread-local scratch — no `Ord`, `Hash`, or
/// `ValueIndex` bound, so `FiniteDomain<String>` still compiles.
pub fn propagate_gac_core<D: Domain>(
    scope: &[VarId],
    sentinel: Option<&D::Value>,
    variables: &mut [Variable<D>],
    depth: usize,
    gac_id: Option<u32>,
) -> Revision
where
    D::Value: 'static,
{
    GAC_CORE_CALLS.fetch_add(1, Ordering::Relaxed);
    with_scratch::<D::Value, _>(|s| propagate_inner(scope, sentinel, variables, depth, gac_id, s))
}

fn propagate_inner<D: Domain>(
    scope: &[VarId],
    sentinel: Option<&D::Value>,
    variables: &mut [Variable<D>],
    depth: usize,
    gac_id: Option<u32>,
    s: &mut GacScratch<D::Value>,
) -> Revision
where
    D::Value: 'static,
{
    // Advance the value→index fast-path generation (Beat 1). Every stamped
    // entry from a prior call is now stale in O(1); on the rare u32 wrap we
    // clear the stamps once and restart at 1 (0 means "never written").
    s.cur_gen = s.cur_gen.wrapping_add(1);
    if s.cur_gen == 0 {
        s.val_index_gen.iter_mut().for_each(|g| *g = 0);
        s.cur_gen = 1;
    }

    // ----- Participant + assigned-value collection -----
    s.assigned_ns.clear();
    for &v in scope {
        let dom = &variables[v as usize].domain;
        if dom.is_empty() {
            return Revision::Unsatisfiable;
        }
        if let Some(val) = dom.singleton_value()
            && sentinel != Some(&val)
        {
            s.assigned_ns.push(val);
        }
    }

    s.participants.clear();
    s.has_sentinel.clear();
    for (i, &v) in scope.iter().enumerate() {
        let dom = &variables[v as usize].domain;
        if dom.is_singleton() {
            continue;
        }
        let dom_has_sentinel = sentinel.map(|sv| dom.contains(sv)).unwrap_or(false);
        let non_sentinel_count = dom.size() - dom_has_sentinel as usize;
        if non_sentinel_count == 0 {
            // {sentinel}-only domain: committed to escape, drops out.
            continue;
        }
        s.participants.push(i);
        s.has_sentinel.push(dom_has_sentinel);
    }

    if s.participants.is_empty() {
        return Revision::Unchanged;
    }
    let n_vars = s.participants.len();

    // Plain all-different: below the threshold, singleton removal (which the
    // caller performs) captures everything GAC would — skip the machinery.
    if sentinel.is_none() && n_vars < GAC_MIN_PARTICIPANTS {
        return Revision::Unchanged;
    }

    // ----- Value universe + bipartite adjacency -----
    s.all_vals.clear();
    reset_adj(&mut s.adj, n_vars);
    for pu in 0..n_vars {
        let var_id = scope[s.participants[pu]] as usize;
        for val in variables[var_id].domain.iter() {
            if sentinel == Some(&val) {
                continue;
            }
            if s.assigned_ns.contains(&val) {
                continue;
            }
            // Value→index resolution. Integer values in range take the O(1)
            // generation-stamped reverse map; everything else falls back to
            // the O(n_vals) `position` scan. Both dedup exactly (INV-B) and
            // agree on the slot numbering, so the two paths are observably
            // identical — the map is a pure accelerator over `all_vals`.
            let vi = match fast_index(&val) {
                Some(k) => {
                    if k >= s.val_index.len() {
                        s.val_index.resize(k + 1, 0);
                        s.val_index_gen.resize(k + 1, 0);
                    }
                    if s.val_index_gen[k] == s.cur_gen {
                        s.val_index[k]
                    } else {
                        let idx = s.all_vals.len() as u32;
                        s.all_vals.push(val);
                        s.val_index[k] = idx;
                        s.val_index_gen[k] = s.cur_gen;
                        idx
                    }
                }
                None => match s.all_vals.iter().position(|x| *x == val) {
                    Some(k) => k as u32,
                    None => {
                        s.all_vals.push(val);
                        (s.all_vals.len() - 1) as u32
                    }
                },
            };
            s.adj[pu].push(vi);
        }
    }

    // All non-sentinel values consumed by assigned singletons.
    if s.all_vals.is_empty() {
        return finish_all_consumed::<D>(scope, sentinel, variables, depth, s);
    }
    let n_vals = s.all_vals.len();

    // ----- Warm-start matching from the per-constraint cache -----
    s.match_u.clear();
    s.match_u.resize(n_vars, NONE);
    s.match_v.clear();
    s.match_v.resize(n_vals, NONE);
    s.dist.clear();
    s.dist.resize(n_vars, 0);

    if let Some(id) = gac_id
        && let Some(cvec) = s.cache.get(&id)
    {
        for pu in 0..n_vars {
            let Some(Some(cv)) = cvec.get(s.participants[pu]) else {
                continue;
            };
            // Re-resolve the cached value against THIS call's `all_vals`
            // (built just above, same generation), via the same fast path.
            // A cached value not present in the current universe → skip, the
            // warm start is a pure hint (INV-G).
            let vi = match fast_index(cv) {
                Some(k) if k < s.val_index.len() && s.val_index_gen[k] == s.cur_gen => {
                    s.val_index[k]
                }
                Some(_) => continue,
                None => match s.all_vals.iter().position(|x| x == cv) {
                    Some(k) => k as u32,
                    None => continue,
                },
            };
            if s.match_v[vi as usize] == NONE && s.adj[pu].contains(&vi) {
                s.match_u[pu] = vi;
                s.match_v[vi as usize] = pu as u32;
            }
        }
    }

    hopcroft_karp(
        n_vars,
        n_vals,
        &s.adj,
        &mut s.match_u,
        &mut s.match_v,
        &mut s.dist,
        &mut s.queue,
    );

    // ----- Coverage: sentinel-less participants must be matched -----
    for pu in 0..n_vars {
        if s.match_u[pu] == NONE && !s.has_sentinel[pu] {
            return Revision::Unsatisfiable;
        }
    }

    // Persist the fresh matching for the next call's warm start.
    if let Some(id) = gac_id {
        if s.cache.len() > CACHE_CAP {
            s.cache.clear();
        }
        let cvec = s.cache.entry(id).or_default();
        cvec.clear();
        cvec.resize(scope.len(), None);
        for pu in 0..n_vars {
            if s.match_u[pu] != NONE {
                cvec[s.participants[pu]] = Some(s.all_vals[s.match_u[pu] as usize].clone());
            }
        }
    }

    // ----- Residual graph -----
    let total_nodes = n_vars + n_vals;
    reset_adj(&mut s.res_adj, total_nodes);
    for u in 0..n_vars {
        let matched_vi = s.match_u[u];
        for &vi in &s.adj[u] {
            let val_node = (n_vars as u32) + vi;
            if vi == matched_vi {
                s.res_adj[val_node as usize].push(u as u32);
            } else {
                s.res_adj[u].push(val_node);
            }
        }
    }

    // ----- Reachability from free vertices -----
    s.reachable.clear();
    s.reachable.resize(total_nodes, false);
    s.bfs.clear();
    for vi in 0..n_vals {
        if s.match_v[vi] == NONE {
            let node = n_vars + vi;
            s.reachable[node] = true;
            s.bfs.push(node as u32);
        }
    }
    for pu in 0..n_vars {
        if s.match_u[pu] == NONE && s.has_sentinel[pu] {
            s.reachable[pu] = true;
            s.bfs.push(pu as u32);
        }
    }
    let mut head = 0;
    while head < s.bfs.len() {
        let node = s.bfs[head] as usize;
        head += 1;
        for i in 0..s.res_adj[node].len() {
            let next = s.res_adj[node][i] as usize;
            if !s.reachable[next] {
                s.reachable[next] = true;
                s.bfs.push(next as u32);
            }
        }
    }

    // ----- SCCs -----
    resize_tarjan(s, total_nodes);
    tarjan_scc(
        total_nodes,
        &s.res_adj,
        &mut s.t_index,
        &mut s.t_lowlink,
        &mut s.t_onstack,
        &mut s.t_scc,
        &mut s.t_stack,
        &mut s.t_call,
    );

    // ----- Prune -----
    let mut changed = false;

    // Phase 1: assigned-singleton non-sentinel values were excluded from the
    // graph; remove them from every participant's live domain.
    if !s.assigned_ns.is_empty() {
        for pu in 0..n_vars {
            let var_id = scope[s.participants[pu]] as usize;
            for k in 0..s.assigned_ns.len() {
                let val = s.assigned_ns[k].clone();
                if variables[var_id].prune(&val, depth) {
                    changed = true;
                }
            }
            if variables[var_id].domain.is_empty() {
                return Revision::Unsatisfiable;
            }
        }
    }

    // Phase 2: Régin SCC pruning — drop unmatched (var, val) edges crossing SCC
    // boundaries and not reachable from a free vertex.
    for pu in 0..n_vars {
        let var_id = scope[s.participants[pu]] as usize;
        let matched_vi = s.match_u[pu];
        for i in 0..s.adj[pu].len() {
            let vi = s.adj[pu][i];
            if vi == matched_vi {
                continue;
            }
            let val_node = n_vars + vi as usize;
            if s.t_scc[pu] == s.t_scc[val_node] || s.reachable[val_node] {
                continue;
            }
            let val = s.all_vals[vi as usize].clone();
            if variables[var_id].prune(&val, depth) {
                changed = true;
            }
        }
        if variables[var_id].domain.is_empty() {
            return Revision::Unsatisfiable;
        }
    }

    if changed {
        Revision::Changed
    } else {
        Revision::Unchanged
    }
}

/// Handle the degenerate case where the value universe is empty (every
/// non-sentinel value was consumed by an assigned singleton).
fn finish_all_consumed<D: Domain>(
    scope: &[VarId],
    sentinel: Option<&D::Value>,
    variables: &mut [Variable<D>],
    depth: usize,
    s: &mut GacScratch<D::Value>,
) -> Revision {
    // Plain variant: an unassigned variable with no available value is UNSAT.
    if sentinel.is_none() {
        return Revision::Unsatisfiable;
    }
    // Sentinel variant: participants survive only via the escape valve. Any
    // participant without a sentinel is unsatisfiable; the rest have their
    // (now-forbidden) assigned values pruned.
    for pu in 0..s.participants.len() {
        if !s.has_sentinel[pu] {
            return Revision::Unsatisfiable;
        }
    }
    let mut changed = false;
    for pu in 0..s.participants.len() {
        let var_id = scope[s.participants[pu]] as usize;
        for k in 0..s.assigned_ns.len() {
            let val = s.assigned_ns[k].clone();
            if variables[var_id].prune(&val, depth) {
                changed = true;
            }
        }
        if variables[var_id].domain.is_empty() {
            return Revision::Unsatisfiable;
        }
    }
    if changed {
        Revision::Changed
    } else {
        Revision::Unchanged
    }
}

fn resize_tarjan<V>(s: &mut GacScratch<V>, n: usize) {
    s.t_index.resize(n, NONE);
    s.t_lowlink.resize(n, 0);
    s.t_onstack.clear();
    s.t_onstack.resize(n, false);
    s.t_scc.resize(n, NONE);
}
