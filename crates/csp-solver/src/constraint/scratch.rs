//! Thread-local scratch pool for n-ary all-different singleton collection.
//!
//! The plain [`AllDifferent`](super::all_different::AllDifferent) and the
//! small-scope [`AllDifferentExcept`](super::all_different_except::AllDifferentExcept)
//! path both take a *snapshot* of which scope variables are singletons before
//! pruning peers. Allocating that `Vec<(VarId, Value)>` on every `revise()` was
//! the dominant malloc source on generation / large-board workloads (86% / 75%
//! / ~55% on gen_holedig / sudoku16 / futoshiki7, per the L26 profile). This
//! pool reuses one thread-local buffer per value type instead, preserving the
//! snapshot-then-prune semantics **exactly** (byte-identical domains) — it is a
//! pure allocation optimization, not a behavioral change.
//!
//! The buffer is borrowed only for the duration of the supplied closure and
//! released before it returns; callers MUST NOT hold it across a re-entrant
//! call into another scratch-borrowing path — notably
//! [`propagate_gac_core`](crate::solver::gac::propagate_gac_core), which
//! borrows its own thread-local. It is a distinct structure from that core's
//! `assigned_ns`, so the two never alias.

use std::any::Any;
use std::cell::RefCell;

use super::traits::VarId;

thread_local! {
    /// One type-erased singleton buffer per thread. Re-materializes only if a
    /// thread revises all-different constraints over different value types.
    static SINGLETON_BUF: RefCell<Box<dyn Any>> = RefCell::new(Box::new(()));
}

/// Run `f` with a cleared, reused thread-local `Vec<(VarId, V)>` scratch.
///
/// The buffer is cleared on entry and the borrow is released when `f` returns.
pub(crate) fn with_singleton_buf<V, R>(f: impl FnOnce(&mut Vec<(VarId, V)>) -> R) -> R
where
    V: 'static,
{
    SINGLETON_BUF.with(|slot| {
        let mut b = slot.borrow_mut();
        if !b.is::<Vec<(VarId, V)>>() {
            *b = Box::new(Vec::<(VarId, V)>::new());
        }
        let buf = b.downcast_mut::<Vec<(VarId, V)>>().unwrap();
        buf.clear();
        f(buf)
    })
}
