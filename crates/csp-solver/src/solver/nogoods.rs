//! Bounded nogood learning store with LRU eviction.
//!
//! Tests: `tests/nogoods.rs`.

use std::collections::VecDeque;

use crate::constraint::VarId;

/// A bounded store of recorded nogoods (partial assignments known to be
/// unsatisfiable).
///
/// Each nogood is a set of `(VarId, Value)` pairs.  The store has both a
/// per-nogood length bound (`max_length`) and a total capacity (`max_entries`).
/// When capacity is exceeded, the oldest entry is evicted (LRU / FIFO).
///
/// An auxiliary `var_index` maps each variable id to the set of nogood indices
/// that mention it, enabling fast lookup during the `is_nogood` check.
#[derive(Debug, Clone)]
pub struct NogoodStore<V: Clone + PartialEq> {
    /// Maximum number of variable-value pairs per nogood.
    max_length: usize,
    /// Maximum number of nogoods stored.
    max_entries: usize,
    /// Ordered collection of nogoods (front = oldest).
    store: VecDeque<Vec<(VarId, V)>>,
    /// For each variable, the set of nogood indices (into `store`) that mention it.
    /// Indices are absolute; they are adjusted on eviction.
    var_index: Vec<Vec<usize>>,
}

impl<V: Clone + PartialEq> NogoodStore<V> {
    /// Create a new empty nogood store.
    ///
    /// * `max_length` — nogoods with more than this many entries are silently
    ///   dropped by `record`.
    /// * `max_entries` — when this capacity is exceeded the oldest nogood is
    ///   evicted.
    /// * `num_vars` — number of CSP variables (used to size the var index).
    pub fn new(max_length: usize, max_entries: usize, num_vars: usize) -> Self {
        Self {
            max_length,
            max_entries,
            store: VecDeque::new(),
            var_index: vec![Vec::new(); num_vars],
        }
    }

    /// Record a new nogood.
    ///
    /// If the nogood's length exceeds `max_length`, it is silently ignored.
    /// If the store is at capacity, the oldest entry is evicted before
    /// insertion (LRU/FIFO).
    pub fn record(&mut self, conflict: &[(VarId, V)]) {
        if conflict.len() > self.max_length {
            return;
        }

        // Evict oldest if at capacity.
        if self.store.len() >= self.max_entries {
            self.evict_oldest();
        }

        let idx = self.store.len();
        self.store.push_back(conflict.to_vec());

        // Update var_index for the new entry.
        for &(var, _) in conflict {
            let v = var as usize;
            if v < self.var_index.len() {
                self.var_index[v].push(idx);
            }
        }
    }

    /// Check whether assigning `var = val` would complete any recorded nogood
    /// given the current (partial) `assignment`.
    ///
    /// Returns `true` if there exists a nogood where:
    /// 1. `(var, val)` appears in the nogood, AND
    /// 2. every other `(v, w)` in the nogood has `assignment[v] == Some(w)`.
    pub fn is_nogood(&self, var: VarId, val: &V, assignment: &[Option<V>]) -> bool {
        let v = var as usize;
        if v >= self.var_index.len() {
            return false;
        }

        for &ng_idx in &self.var_index[v] {
            let nogood = &self.store[ng_idx];

            // Check that (var, val) is actually in this nogood.
            let has_pair = nogood.iter().any(|(nv, nval)| *nv == var && nval == val);
            if !has_pair {
                continue;
            }

            // Check all other entries match the current assignment.
            let all_match = nogood.iter().all(|(nv, nval)| {
                if *nv == var {
                    // This is the entry we're hypothetically assigning.
                    true
                } else {
                    match &assignment[*nv as usize] {
                        Some(av) => av == nval,
                        None => false, // unassigned → can't complete the nogood
                    }
                }
            });

            if all_match {
                return true;
            }
        }

        false
    }

    /// Clear all recorded nogoods and reset the var index.
    pub fn clear(&mut self) {
        self.store.clear();
        for idx_list in &mut self.var_index {
            idx_list.clear();
        }
    }

    /// Number of nogoods currently stored.
    pub fn len(&self) -> usize {
        self.store.len()
    }

    /// Whether the store is empty.
    pub fn is_empty(&self) -> bool {
        self.store.is_empty()
    }

    // -----------------------------------------------------------------------
    // Internal
    // -----------------------------------------------------------------------

    /// Evict the oldest (front) nogood and fixup `var_index`.
    fn evict_oldest(&mut self) {
        if let Some(evicted) = self.store.pop_front() {
            // After pop_front, all remaining indices shifted down by 1.
            // Rebuild var_index from scratch (simple and correct).
            // For small stores this is fine; for large stores a generation
            // counter would be better, but max_entries is bounded.
            self.rebuild_var_index_after_eviction(&evicted);
        }
    }

    /// Rebuild `var_index` after evicting the front element.
    ///
    /// All indices in `var_index` that pointed to the evicted entry (index 0
    /// before pop) must be removed, and all remaining indices decremented by 1.
    fn rebuild_var_index_after_eviction(&mut self, _evicted: &[(VarId, V)]) {
        // Full rebuild — simple and O(total stored pairs).
        for idx_list in &mut self.var_index {
            idx_list.clear();
        }
        for (ng_idx, nogood) in self.store.iter().enumerate() {
            for &(var, _) in nogood {
                let v = var as usize;
                if v < self.var_index.len() {
                    self.var_index[v].push(ng_idx);
                }
            }
        }
    }
}
