//! Conflict-history weighting (CHS / ERWA) for dynamic variable ordering.
//!
//! Replaces the frozen `constraint_weights ≡ 1.0` (which is why `Ordering::Mrv`
//! reduces to plain min-remaining-value) with an *exponential recency-weighted
//! average* per constraint, following Habet & Terrioux's Conflict-History
//! Search (CHS, J. Heuristics 2021).
//!
//! On every conflict (a failed check or a propagation domain wipe-out) the
//! offending constraint `c` is bumped:
//!
//! ```text
//! r        = 1 / (conflicts − last_conflict[c] + 1)
//! q[c]     ← (1 − α)·q[c] + α·r
//! α        ← max(α_min, α − α_decay)      (decays 0.4 → 0.06)
//! ```
//!
//! Branching then minimises `dom(x) / Σ_{c ∈ scope(x)} q[c]` — constraints
//! that failed recently and often dominate the denominator, pulling their
//! variables to the front of the search.
//!
//! Weights are *search-only* state: they never touch the BBNF monotonic /
//! lattice propagation path (which never enters backtracking search).

/// Initial per-constraint score. Starting at `1.0` (rather than CHS's `0.0`)
/// makes the denominator `Σ q` behave like weighted-degree on the first
/// descent — i.e. MRV on a degree-uniform graph such as sudoku — before any
/// conflict has been observed, avoiding the divide-by-near-zero degeneracy of
/// a cold `q ≡ 0` start. Once conflicts accrue the ERWA update takes over.
const INITIAL_Q: f64 = 1.0;

/// Starting ERWA step size (favours recent conflicts heavily).
const ALPHA_INIT: f64 = 0.4;
/// Per-conflict decay applied to `α`.
const ALPHA_DECAY: f64 = 1e-6;
/// Floor for `α` (long-run smoothing factor).
const ALPHA_MIN: f64 = 0.06;

/// Dynamic conflict-history state threaded through backtracking search.
#[derive(Debug, Clone)]
pub struct ConflictHistory {
    /// `q[c]` — CHS score per constraint. Read by `Ordering::{Chs,Mrv}`.
    weights: Vec<f64>,
    /// `last_conflict[c]` — global conflict count at `c`'s previous failure.
    last_conflict: Vec<u64>,
    /// Global conflict counter (monotonic across restarts).
    conflicts: u64,
    /// Current ERWA step size.
    alpha: f64,
    /// When `false`, `bump` is a no-op and weights stay at `INITIAL_Q`
    /// (keeps the frozen-weight `Mrv` scan for non-CHS orderings).
    enabled: bool,
}

impl ConflictHistory {
    /// Create history for `num_constraints`. `enabled` gates the ERWA update;
    /// when disabled the weights are the frozen `1.0` vector the pre-CHS code
    /// used, so `Ordering::Mrv` stays bit-for-bit the frozen-weight scan.
    pub fn new(num_constraints: usize, enabled: bool) -> Self {
        Self {
            weights: vec![INITIAL_Q; num_constraints],
            last_conflict: vec![0; num_constraints],
            conflicts: 0,
            alpha: ALPHA_INIT,
            enabled,
        }
    }

    /// Create history seeded from an existing weight vector (e.g. the
    /// `constraint_weights` built by `finalize()`). Behaves like `new` when
    /// the seed is the conventional all-`1.0` vector.
    pub fn from_seed(seed: &[f64], enabled: bool) -> Self {
        Self {
            weights: seed.to_vec(),
            last_conflict: vec![0; seed.len()],
            conflicts: 0,
            alpha: ALPHA_INIT,
            enabled,
        }
    }

    /// Current weight vector, consumed by the ordering heuristic.
    #[inline]
    pub fn weights(&self) -> &[f64] {
        &self.weights
    }

    /// Total conflicts recorded so far (used by the restart driver).
    #[inline]
    pub fn conflicts(&self) -> u64 {
        self.conflicts
    }

    /// Register a conflict caused by constraint `ci` and apply the ERWA
    /// update. A no-op when disabled.
    #[inline]
    pub fn bump(&mut self, ci: usize) {
        if !self.enabled {
            self.conflicts += 1;
            return;
        }
        self.conflicts += 1;
        let since = self.conflicts - self.last_conflict[ci] + 1;
        let r = 1.0 / since as f64;
        self.weights[ci] = (1.0 - self.alpha) * self.weights[ci] + self.alpha * r;
        self.last_conflict[ci] = self.conflicts;
        if self.alpha > ALPHA_MIN {
            self.alpha = (self.alpha - ALPHA_DECAY).max(ALPHA_MIN);
        }
    }
}
