//! Reusable per-thread scratch substrate for the GAC core.
//!
//! Value-generic, caller-owned buffer pooling — the second "caller-owned
//! scratch" primitive beside [`super::matching`]. The propagator pipeline in
//! [`super`] touches this only through [`with_scratch`] and direct field
//! access, so the substrate references no propagator logic.

use std::any::Any;
use std::cell::RefCell;
use std::collections::HashMap;

use super::matching::NONE;

/// Per-thread, per-value-type reusable working set for the GAC core.
///
/// Generic in the value type only for the value universe and matching cache;
/// every other buffer is integer-indexed and value-agnostic.
pub(super) struct GacScratch<V> {
    pub(super) participants: Vec<usize>,
    pub(super) has_sentinel: Vec<bool>,
    pub(super) assigned_ns: Vec<V>,
    pub(super) all_vals: Vec<V>,
    pub(super) adj: Vec<Vec<u32>>,
    pub(super) match_u: Vec<u32>,
    pub(super) match_v: Vec<u32>,
    pub(super) dist: Vec<u32>,
    pub(super) queue: Vec<u32>,
    pub(super) res_adj: Vec<Vec<u32>>,
    pub(super) reachable: Vec<bool>,
    pub(super) bfs: Vec<u32>,
    pub(super) t_index: Vec<u32>,
    pub(super) t_lowlink: Vec<u32>,
    pub(super) t_onstack: Vec<bool>,
    pub(super) t_scc: Vec<u32>,
    pub(super) t_stack: Vec<u32>,
    pub(super) t_call: Vec<(u32, u32)>,
    pub(super) cache: HashMap<u32, Vec<Option<V>>>,
    /// Generation-stamped reverse map for the integer fast path (Beat 1).
    ///
    /// When `D::Value` is a small non-negative integer, `val_index[k]` holds
    /// the `all_vals` slot for value `k` **iff** `val_index_gen[k] == cur_gen`.
    /// The stamp makes the per-call reset O(1) (bump `cur_gen`) so the buffer
    /// never leaks a prior call's numbering across a value-universe shrink —
    /// see [`INV-A`](self). Non-integer / out-of-range values fall back to the
    /// generic `PartialEq` `position` scan, so no `Hash`/`Ord`/`ValueIndex`
    /// bound is introduced.
    pub(super) val_index: Vec<u32>,
    pub(super) val_index_gen: Vec<u32>,
    pub(super) cur_gen: u32,
}

/// Upper bound on the integer key the value→index fast path will address.
/// A value above this (or negative, or non-integer) uses the `position`-scan
/// fallback, bounding `val_index`'s worst-case footprint. Live GAC value
/// universes are tiny (`BitsetDomain` is 0..128; assignment columns 0..n_cols),
/// so the cap is never approached in practice.
pub(super) const MAX_FAST_INDEX: usize = 1 << 20;

/// If `v` is one of the supported integer types and lands in
/// `0..=MAX_FAST_INDEX`, return it as a reverse-map key; otherwise `None`
/// (caller falls back to the `PartialEq` scan). Uses the `Any` blanket impl
/// available to every `'static` type, so it adds **no** trait bound to the
/// generic value — `FiniteDomain<String>` still routes entirely through the
/// scan and compiles unchanged.
pub(super) fn fast_index<V: 'static>(v: &V) -> Option<usize> {
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

pub(super) fn with_scratch<V, R>(f: impl FnOnce(&mut GacScratch<V>) -> R) -> R
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

pub(super) fn resize_tarjan<V>(s: &mut GacScratch<V>, n: usize) {
    s.t_index.resize(n, NONE);
    s.t_lowlink.resize(n, 0);
    s.t_onstack.clear();
    s.t_onstack.resize(n, false);
    s.t_scc.resize(n, NONE);
}
