//! Cooperative cancellation token.
//!
//! A cheap, `Clone`-able flag checked at the same cadence as
//! [`crate::SolveConfig::node_budget`] inside every search loop
//! (`solver::backtrack`, `solver::backjump`, `solver::optimize`). Unlike the
//! node budget — which is an internal, self-imposed cap — this is an
//! *external* signal: a caller (e.g. the PyO3 boundary, on behalf of an
//! `asyncio.wait_for` timeout) can flip it from another thread while the
//! search is running on a `Python::allow_threads`-released worker thread,
//! and the next node-budget-cadence check will unwind the recursion cleanly.
//!
//! Deliberately *not* the same flag/field as `budget_exceeded` — the two are
//! distinct outcomes (self-imposed cap vs. externally requested stop) and
//! collapsing them would repeat the exact "conflated failure modes" mistake
//! flagged elsewhere in this codebase's own solve-result plumbing.

use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};

/// A shareable, thread-safe cancellation flag.
///
/// `clone()` is cheap (an `Arc` bump) — hand one clone to the search config
/// and keep another on the calling side to flip when a timeout elapses.
#[derive(Clone, Debug, Default)]
pub struct CancelToken(Arc<AtomicBool>);

impl CancelToken {
    /// Create a new, not-yet-cancelled token.
    pub fn new() -> Self {
        Self(Arc::new(AtomicBool::new(false)))
    }

    /// Request cancellation. Safe to call from any thread, at any time,
    /// including while a search holding a clone of this token is running.
    pub fn cancel(&self) {
        self.0.store(true, Ordering::Relaxed);
    }

    /// Check whether cancellation has been requested.
    #[inline]
    pub fn is_cancelled(&self) -> bool {
        self.0.load(Ordering::Relaxed)
    }
}
