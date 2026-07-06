//! Luby restart schedule.
//!
//! The Luby sequence `1,1,2,1,1,2,4,1,1,2,1,1,2,4,8,…` is log-optimal for
//! restarting a Las-Vegas algorithm whose runtime distribution is unknown
//! (Luby, Sinclair & Zuckerman 1993) — the canonical CSP/SAT cure for
//! heavy-tailed search. The i-th run is given a conflict budget of
//! `luby(i) · unit`; on exhaustion the search unwinds to the root and
//! re-descends, *retaining* the CHS weights, phase memory, and restart
//! nogoods accumulated so far (otherwise a restart merely repeats itself).
//!
//! Values are produced by Knuth's "reluctant doubling" recurrence — O(1) per
//! step, no table, no recursion.

/// Default per-restart base unit, in conflicts. The k-th run runs for
/// `luby(k) · UNIT` conflicts before cutting off.
pub const DEFAULT_UNIT: u64 = 100;

/// Streaming generator for the Luby unit sequence.
#[derive(Debug, Clone)]
pub struct Luby {
    u: u64,
    v: u64,
}

impl Luby {
    /// Fresh generator positioned before the first term.
    pub fn new() -> Self {
        Self { u: 1, v: 1 }
    }

    /// Next term of the Luby sequence.
    #[inline]
    pub fn next_term(&mut self) -> u64 {
        let term = self.v;
        if self.u & self.u.wrapping_neg() == self.v {
            self.u += 1;
            self.v = 1;
        } else {
            self.v *= 2;
        }
        term
    }
}

impl Default for Luby {
    fn default() -> Self {
        Self::new()
    }
}

impl Iterator for Luby {
    type Item = u64;
    #[inline]
    fn next(&mut self) -> Option<u64> {
        Some(self.next_term())
    }
}
