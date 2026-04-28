//! Multi-criteria rewrite-rule ranker.
//!
//! Given a slice of [`Rule`]s, [`rank`] reorders them in place by a
//! weighted score across six dimensions. [`select_top_k`] returns the
//! `k` highest-scoring rules without mutating the input.
//!
//! # The six axes
//!
//! | Axis        | Direction        | Computed from                                         |
//! |-------------|------------------|-------------------------------------------------------|
//! | frequency   | higher = better  | `rule.frequency` (fires per 1k input bytes)           |
//! | cost_delta  | lower = better   | `rule.cost_delta` (negative = win on cost model)      |
//! | generality  | higher = better  | `#distinct alphabets ∩ #distinct rules` in lhs+rhs    |
//! | similarity  | distance only    | Jaccard distance of leaf multiset vs already-accepted |
//! | novelty     | higher = better  | `1 - mean(similarity)` across already-accepted set    |
//! | size        | tie-break        | `lhs.ast_size() + rhs.ast_size()`                     |
//!
//! The dispatcher computes a weighted sum (axes scaled to [0, 1]) and
//! sorts descending. `RankConfig::default()` uses uniform weights;
//! consumers tune for the corpus at hand.

use super::{Atom, Rule};

/// Per-axis weights for [`rank`]. All weights are non-negative and
/// summed; a 0 weight disables the axis.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct RankConfig {
    /// Weight on `frequency` axis.
    pub w_frequency: f64,
    /// Weight on `cost_delta` axis.
    pub w_cost: f64,
    /// Weight on `generality` axis.
    pub w_generality: f64,
    /// Weight on `novelty` axis.
    pub w_novelty: f64,
    /// Weight on `size` axis (tie-breaker; usually small).
    pub w_size: f64,
}

impl Default for RankConfig {
    fn default() -> Self {
        Self {
            w_frequency: 1.0,
            w_cost: 1.0,
            w_generality: 1.0,
            w_novelty: 0.5,
            w_size: 0.1,
        }
    }
}

/// Rank `rules` in place. Highest-scoring rule first. Stable.
///
/// The computation happens in two passes:
///
/// 1. **Static-axis scoring** — compute frequency, cost, generality,
///    size for each rule (axes that don't depend on inter-rule order).
/// 2. **Greedy diversity sort** — at each output slot, pick the
///    unrlaced rule with the highest static-score + novelty bonus
///    against the already-placed prefix. This is the standard MMR
///    (Maximal Marginal Relevance) shape.
pub fn rank(rules: &mut [Rule], cfg: &RankConfig) {
    if rules.is_empty() {
        return;
    }

    // ── Pass 1: static scores ──────────────────────────────────────
    let mut static_scores: Vec<f64> = Vec::with_capacity(rules.len());
    let max_freq = rules.iter().map(|r| r.frequency as f64).fold(0.0, f64::max);
    let max_cost_neg = rules
        .iter()
        .map(|r| (-r.cost_delta) as f64)
        .fold(0.0, f64::max);
    let max_size = rules
        .iter()
        .map(|r| (r.lhs.ast_size() + r.rhs.ast_size()) as f64)
        .fold(1.0_f64, f64::max);
    let max_generality = rules
        .iter()
        .map(|r| generality(r) as f64)
        .fold(1.0_f64, f64::max);

    for r in rules.iter() {
        let freq = if max_freq > 0.0 { (r.frequency as f64) / max_freq } else { 0.0 };
        let cost = if max_cost_neg > 0.0 {
            ((-r.cost_delta) as f64).max(0.0) / max_cost_neg
        } else {
            0.0
        };
        let gen_ = (generality(r) as f64) / max_generality;
        // Smaller size scores higher: invert.
        let size_norm = 1.0 - ((r.lhs.ast_size() + r.rhs.ast_size()) as f64) / max_size;
        let s = cfg.w_frequency * freq
            + cfg.w_cost * cost
            + cfg.w_generality * gen_
            + cfg.w_size * size_norm;
        static_scores.push(s);
    }

    // ── Pass 2: greedy MMR with novelty against the placed prefix.
    let n = rules.len();
    let mut placed: Vec<usize> = Vec::with_capacity(n);
    let mut remaining: Vec<usize> = (0..n).collect();

    while !remaining.is_empty() {
        let mut best_idx_in_remaining = 0;
        let mut best_score = f64::NEG_INFINITY;

        for (i, &orig_idx) in remaining.iter().enumerate() {
            let novelty = if placed.is_empty() {
                1.0
            } else {
                let mean_sim: f64 = placed
                    .iter()
                    .map(|&j| jaccard_similarity(&rules[orig_idx], &rules[j]))
                    .sum::<f64>()
                    / (placed.len() as f64);
                1.0 - mean_sim
            };
            let score = static_scores[orig_idx] + cfg.w_novelty * novelty;
            if score > best_score {
                best_score = score;
                best_idx_in_remaining = i;
            }
        }

        let chosen = remaining.swap_remove(best_idx_in_remaining);
        placed.push(chosen);
    }

    // Apply the permutation in place via a cycle-decomposition swap.
    // Each cycle visits its elements once; total work is O(n) swaps.
    // The auxiliary `slot_of` map records where each original index
    // currently lives.
    let mut slot_of: Vec<usize> = (0..n).collect();
    // `target[i]` = the original index that should end up at slot i.
    let target = placed;
    for i in 0..n {
        let want = target[i];
        // Find the current slot of `want`; swap whatever's there into i.
        let cur = slot_of
            .iter()
            .position(|&v| v == want)
            .expect("target is a permutation of 0..n");
        if cur == i {
            continue;
        }
        rules.swap(i, cur);
        slot_of.swap(i, cur);
    }
}

/// Top-k selection by [`rank`] score *without* mutating the input.
/// Equivalent to ranking a copy and taking the first `k` references,
/// but skips the placeholder dance.
pub fn select_top_k<'a>(rules: &'a [Rule], k: usize) -> Vec<&'a Rule> {
    if rules.is_empty() || k == 0 {
        return Vec::new();
    }
    let cfg = RankConfig::default();
    let mut indices: Vec<usize> = (0..rules.len()).collect();

    let max_freq = rules.iter().map(|r| r.frequency as f64).fold(0.0, f64::max);
    let max_cost_neg = rules
        .iter()
        .map(|r| (-r.cost_delta) as f64)
        .fold(0.0, f64::max);
    let max_size = rules
        .iter()
        .map(|r| (r.lhs.ast_size() + r.rhs.ast_size()) as f64)
        .fold(1.0_f64, f64::max);
    let max_generality = rules
        .iter()
        .map(|r| generality(r) as f64)
        .fold(1.0_f64, f64::max);

    let static_score = |i: usize| -> f64 {
        let r = &rules[i];
        let freq = if max_freq > 0.0 { (r.frequency as f64) / max_freq } else { 0.0 };
        let cost = if max_cost_neg > 0.0 {
            ((-r.cost_delta) as f64).max(0.0) / max_cost_neg
        } else {
            0.0
        };
        let gen_ = (generality(r) as f64) / max_generality;
        let size_norm = 1.0 - ((r.lhs.ast_size() + r.rhs.ast_size()) as f64) / max_size;
        cfg.w_frequency * freq
            + cfg.w_cost * cost
            + cfg.w_generality * gen_
            + cfg.w_size * size_norm
    };

    let mut placed: Vec<usize> = Vec::with_capacity(k.min(rules.len()));
    while placed.len() < k.min(rules.len()) && !indices.is_empty() {
        let mut best_pos = 0usize;
        let mut best_score = f64::NEG_INFINITY;
        for (pos, &orig) in indices.iter().enumerate() {
            let novelty = if placed.is_empty() {
                1.0
            } else {
                let m: f64 = placed
                    .iter()
                    .map(|&j| jaccard_similarity(&rules[orig], &rules[j]))
                    .sum::<f64>()
                    / (placed.len() as f64);
                1.0 - m
            };
            let s = static_score(orig) + cfg.w_novelty * novelty;
            if s > best_score {
                best_score = s;
                best_pos = pos;
            }
        }
        let chosen = indices.swap_remove(best_pos);
        placed.push(chosen);
    }
    placed.into_iter().map(|i| &rules[i]).collect()
}

// ── Per-axis primitives ──────────────────────────────────────────────

/// Generality: # distinct atom-shapes referenced by lhs+rhs. A rule
/// that mentions five atom shapes is 5× more general than one that
/// mentions one.
fn generality(rule: &Rule) -> usize {
    use std::collections::HashSet;
    let mut set: HashSet<Atom> = HashSet::new();
    for a in rule.lhs.atoms().into_iter().chain(rule.rhs.atoms()) {
        set.insert(a);
    }
    set.len().max(1)
}

/// Jaccard similarity (1 - distance) of leaf multisets.
fn jaccard_similarity(a: &Rule, b: &Rule) -> f64 {
    use std::collections::HashSet;
    let a_set: HashSet<Atom> = a.lhs.atoms().into_iter().chain(a.rhs.atoms()).collect();
    let b_set: HashSet<Atom> = b.lhs.atoms().into_iter().chain(b.rhs.atoms()).collect();
    if a_set.is_empty() && b_set.is_empty() {
        return 1.0;
    }
    let inter = a_set.intersection(&b_set).count() as f64;
    let union = a_set.union(&b_set).count() as f64;
    if union == 0.0 { 0.0 } else { inter / union }
}

