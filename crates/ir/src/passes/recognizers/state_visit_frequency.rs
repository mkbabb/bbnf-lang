//! State-visit frequency mining for walker specialisation (AW-III.W4.a).
//!
//! # Architectural role
//!
//! AW-III.W4 ships a general walker-specialisation emitter pass that
//! lowers a [`DtaTable`] into one inlined Rust function with N labelled
//! state arms (per AW-III.md §W4). When `state_count` exceeds
//! [`HOT_BUDGET`] the emitter must split the function into a hot inner
//! loop carrying the most-frequently-visited states inline and a set
//! of `#[cold] #[inline(never)]` siblings that own the cold tail. The
//! split needs an analytic ordering of states by expected visit
//! frequency — that ordering is what this pass produces.
//!
//! The pass is intentionally pluggable per the AW-III invariants: the
//! emitter consumes a `Vec<(StateId, u32)>` and is free to choose its
//! own threshold and its own slicing strategy. This module owns the
//! frequency model, not the partitioning policy.
//!
//! # Frequency model
//!
//! The pass walks the lifted [`DtaTable`] and propagates a per-state
//! visit-frequency estimate from the entry rule outward. Frequency is
//! a unitless integer — the emitter consumes the *ordering*, not the
//! absolute magnitude. The unit (one parse of the entry rule's body
//! at the top of input) is convenient for reasoning about loop
//! multipliers but no consumer reads the absolute value.
//!
//! Per-edge propagation:
//!
//! | DTA shape           | Edge contribution                                 |
//! |---------------------|---------------------------------------------------|
//! | `Seq.children`      | Each child += parent (every child visited once)   |
//! | `ByteDispatch.table`| Each branch += parent × (byte_density / 256)      |
//! | `ByteDispatch.fb`   | Fallback   += parent × (1 - dispatched_density)   |
//! | `AltLinear`         | Each branch += parent × (1 / N) (uniform prior)   |
//! | `Repeat[lo,hi]`     | inner += parent × `repeat_multiplier(lo, hi)`     |
//! | `Ref { target }`    | target += parent (the descent reuses one entry)   |
//! | `ShuntingYard.head` | head   += parent × [`SHUNTING_YARD_MULTIPLIER`]   |
//! | `Minus`             | primary, excluded += parent (probe + run)         |
//! | `WsTrim` / leaves   | no outgoing edges                                 |
//!
//! ## `repeat_multiplier`
//!
//! - `(0, 1)` (optional)        → 1
//! - `(1, 1)` (exact-one)       → 1
//! - `(0, hi)` / `(1, hi)`      → [`REPEAT_BODY_MULTIPLIER`] (≈ 4 — average
//!   non-trivial repeat body iteration count across the JSON / CSS /
//!   Sheets corpora; tuned from samply traces in tranche AO-AP)
//! - `(lo, hi)` with hi finite  → `max(REPEAT_BODY_MULTIPLIER, hi as u32)`
//!
//! # Ref-cycle handling
//!
//! `Ref { target }` edges induce cycles whenever the grammar is left-
//! recursive at the IR level (the lifter does not rewrite recursion).
//! The propagation loop bounds itself with [`MAX_PROPAGATION_ITERS`]
//! and the per-state contribution accumulator saturates at
//! [`u32::MAX`]; both invariants together guarantee termination
//! regardless of cycle topology.
//!
//! Convergence is monotone: each pass's contribution to a state's
//! frequency is `freq[parent] × multiplier`, and `freq[parent]` only
//! grows. The fixed-point bool [`Changed`](Changed) (LLVM-style;
//! matches the codebase invariant for fixed-point loops) breaks the
//! loop when no state's frequency changes during a full sweep.
//!
//! # Determinism
//!
//! Output ordering is total: descending by frequency, ties broken by
//! ascending state id. Two consecutive runs on the same `DtaTable`
//! produce identical `Vec<(StateId, u32)>` — verified by the
//! `determinism` test in `tests/state_visit_frequency.rs`.
//!
//! # `HOT_BUDGET` rationale
//!
//! The default budget of 64 states tracks the LLVM inlining heuristic:
//! `inline-threshold` defaults to 225 cost units, and a typical DTA
//! state lowers to ~3-4 cost units after const-prop. 64 × 3.5 ≈ 224
//! sits right at the LLVM threshold, so a single function with 64
//! states stays inline-eligible. Above that, the cold branches start
//! getting evicted from inline candidates — the emitter takes the cue
//! and splits explicitly via `#[cold] #[inline(never)]`. The constant
//! is exposed as `pub` so the W4.b emitter can re-tune from samply
//! evidence on the actual emitted assembly.

use std::collections::{HashMap, HashSet};

use crate::passes::recognizers::dta::{DtaState, DtaTable, StateId};

// ── Tunable constants ────────────────────────────────────────────────

/// State-count threshold above which the W4 walker emitter splits the
/// state machine into a hot inner loop + `#[cold]` cold tail.
///
/// 64 mirrors the LLVM `inline-threshold` budget for one DTA-arm
/// function: 64 states × ~3.5 cost units / state ≈ 224 ≤ 225 (LLVM
/// default). Beyond the threshold the cold arms start being elided
/// from inline candidates — partitioning preserves the hot path's
/// inline locality at the cost of a call to the cold sibling.
///
/// The W4.b emitter may override this at emission time per the
/// pluggable cost-model invariant; this constant is the default.
pub const HOT_BUDGET: usize = 64;

/// Hard cap on propagation passes. Cycles in the [`DtaState::Ref`]
/// edges would otherwise diverge; this bound combined with `u32`
/// saturation guarantees termination. Empirically the propagation
/// quiesces in 4-6 iterations across BBNF / JSON / CSS / Sheets shapes.
pub const MAX_PROPAGATION_ITERS: usize = 16;

/// Average iteration count for an unbounded `Repeat` body across the
/// shipped corpus. Conservative — the actual count varies by document
/// (a JSON array can have 1 or 10000 elements) but the ordering hint
/// only needs to put loop bodies above non-loop siblings; the absolute
/// value need not match any specific document's traversal.
pub const REPEAT_BODY_MULTIPLIER: u32 = 4;

/// Frequency boost for a [`DtaState::ShuntingYard`] head — the operand
/// state at the bottom of a precedence chain. Sheets `__formula` →
/// `__unary_expr` collapses to a single ShuntingYard whose head is hit
/// once per operand, but every operator iteration re-enters the head.
/// Two is a conservative under-estimate that still puts the head above
/// transparent siblings.
pub const SHUNTING_YARD_MULTIPLIER: u32 = 2;

/// Initial visit-frequency floor. Every state is assumed reachable at
/// least once during a parse so its frequency is non-zero — the
/// ordering hint then only needs to lift hot states above the floor,
/// not lift unreachable states above zero. Unreachable states stay at
/// the floor and naturally fall to the cold tail.
const BASE_FREQUENCY: u32 = 1;

// ── Public entry point ───────────────────────────────────────────────

/// Mine the state-visit frequency ordering from a lifted [`DtaTable`].
///
/// Returns one `(StateId, u32)` pair per state in the table, sorted
/// descending by estimated visit frequency. Ties are broken by
/// ascending state id so the output is deterministic across runs.
///
/// The emitter (W4.b) reads this ordering to decide which states ride
/// the hot inner loop and which lower to `#[cold] #[inline(never)]`
/// siblings when `table.states.len() > HOT_BUDGET`.
///
/// # Algorithm
///
/// See module docstring for the full propagation model. In short: seed
/// every rule entry state with a base frequency, iterate forward
/// propagation across the per-edge weight multipliers until quiescent,
/// then sort.
pub fn compute_state_visit_frequency(table: &DtaTable) -> Vec<(StateId, u32)> {
    let n = table.states.len();
    if n == 0 {
        return Vec::new();
    }

    let mut freq: Vec<u32> = vec![BASE_FREQUENCY; n];

    // Seed every rule's entry state with one extra visit — the entry
    // rule is the natural seed for the topology walk, but the lifter
    // also surfaces non-entry rules through `rule_entries` so cross-
    // rule `Ref` edges land somewhere finite. Boost each rule entry
    // beyond the floor so the propagation has a non-trivial source.
    for &state in table.rule_entries.values() {
        if let Some(slot) = freq.get_mut(state.0 as usize) {
            *slot = slot.saturating_add(BASE_FREQUENCY);
        }
    }

    // Boost the grammar's authoritative entry rule one tier above
    // every other rule entry — every parse begins there, so its
    // descendants accumulate the dominant share of visits.
    let entry_state = table.rule_entry(table.entry);
    if entry_state != StateId::NONE {
        if let Some(slot) = freq.get_mut(entry_state.0 as usize) {
            *slot = slot.saturating_add(REPEAT_BODY_MULTIPLIER);
        }
    }

    // Forward propagation, LLVM-style fixed-point loop.
    for _ in 0..MAX_PROPAGATION_ITERS {
        let mut changed = false;
        for state_idx in 0..n {
            let parent_freq = freq[state_idx];
            if parent_freq == 0 {
                continue;
            }
            let state = &table.states[state_idx];
            for_each_outgoing_edge(state, |child, weight_num, weight_den| {
                let Some(child_slot) = child_index(child, n) else {
                    return;
                };
                let contribution = scale(parent_freq, weight_num, weight_den);
                let combined = freq[child_slot].saturating_add(contribution);
                if combined > freq[child_slot] {
                    freq[child_slot] = combined;
                    changed = true;
                }
            });
        }
        if !changed {
            break;
        }
    }

    // Compose deterministic output: descending by frequency, ties by
    // ascending state id.
    let mut out: Vec<(StateId, u32)> = (0..n)
        .map(|i| (StateId(i as u16), freq[i]))
        .collect();
    out.sort_by(|a, b| b.1.cmp(&a.1).then_with(|| a.0.cmp(&b.0)));
    out
}

// ── Edge enumeration ────────────────────────────────────────────────

/// Resolve a child [`StateId`] into a state-vector index.
///
/// Filters `StateId::NONE` (the byte-dispatch sentinel for slots with
/// no admitted branch) and out-of-range ids (defensive — well-formed
/// tables should not produce these but the lifter currently has no
/// hard guarantee).
#[inline]
fn child_index(child: StateId, total_states: usize) -> Option<usize> {
    if child == StateId::NONE {
        return None;
    }
    let idx = child.0 as usize;
    if idx >= total_states {
        return None;
    }
    Some(idx)
}

/// Saturating multiply-then-divide for a per-edge weight.
///
/// Computes `(parent_freq × num) / den` clamped to `u32::MAX` on
/// overflow. The intermediate `u64` widening keeps every realistic
/// propagation in-bounds; the saturation arm only fires for cyclic
/// states whose frequency has already saturated upstream.
#[inline]
fn scale(parent_freq: u32, num: u32, den: u32) -> u32 {
    debug_assert!(den > 0, "edge weight denominator must be positive");
    let widened = (parent_freq as u64).saturating_mul(num as u64);
    let scaled = widened / (den as u64);
    if scaled > u32::MAX as u64 {
        u32::MAX
    } else {
        scaled as u32
    }
}

/// Enumerate the outgoing edges of `state` and call `visit(child, num, den)`
/// once per edge with the per-edge contribution multiplier
/// `num / den` (parent frequency is multiplied by `num / den` before
/// being added to the child).
///
/// Edge multipliers:
///
/// - `Seq` children: `1/1` each (every child visited per Seq invocation).
/// - `ByteDispatch.table`: `dispatched_count_per_target / 256` each
///   (uniform-prior over the byte alphabet hitting the same branch).
/// - `ByteDispatch.fallback`: `(256 - dispatched_total) / 256`.
/// - `AltLinear` branches: `1 / branch_count` each (uniform prior over
///   which branch matches).
/// - `Repeat`: inner with `repeat_multiplier(lo, hi) / 1`.
/// - `Ref`: target with `1 / 1` (the cross-rule descent is one visit
///   per parent visit).
/// - `ShuntingYard`: head with `SHUNTING_YARD_MULTIPLIER / 1`.
/// - `Minus`: primary and excluded each with `1 / 1` (excluded probed,
///   then primary on success of the probe's complement).
/// - `WsTrim`, `Literal`, `Regex`, `Epsilon`: no outgoing edges.
fn for_each_outgoing_edge<F: FnMut(StateId, u32, u32)>(state: &DtaState, mut visit: F) {
    match state {
        DtaState::Seq { children, .. } => {
            for &child in children {
                visit(child, 1, 1);
            }
        }
        DtaState::ByteDispatch { table, fallback }
        | DtaState::ClassifyByte { table, fallback } => {
            // Sum byte density per target: the more bytes that route to
            // a state, the more frequently the runtime dispatches into
            // it. The fallback receives the residual probability mass.
            // AW-III.W6.3 — ClassifyByte shares the same frequency
            // model as ByteDispatch (both are byte-indexed state-id
            // routers; only the mining provenance differs).
            let mut counts: HashMap<u16, u32> = HashMap::new();
            for &target in table.iter() {
                if target == StateId::NONE {
                    continue;
                }
                *counts.entry(target.0).or_insert(0) += 1;
            }
            let mut dispatched = 0u32;
            for (raw_id, count) in &counts {
                visit(StateId(*raw_id), *count, 256);
                dispatched += *count;
            }
            if let Some(fb) = fallback {
                let residual = 256u32.saturating_sub(dispatched).max(1);
                visit(*fb, residual, 256);
            }
        }
        DtaState::AltLinear { branches } => {
            let n = branches.len().max(1) as u32;
            for &branch in branches {
                visit(branch, 1, n);
            }
        }
        DtaState::Repeat { inner, lo, hi, .. } => {
            visit(*inner, repeat_multiplier(*lo, *hi), 1);
        }
        DtaState::Ref { target, .. } => {
            // Even when the lifter could not pre-resolve the target
            // (`StateId::NONE`), the propagation bails harmlessly via
            // `child_index` — the runtime's late-bound resolution sits
            // in the driver, not in the frequency model.
            visit(*target, 1, 1);
        }
        DtaState::ShuntingYard { head, .. } => {
            visit(*head, SHUNTING_YARD_MULTIPLIER, 1);
        }
        DtaState::Minus { primary, excluded } => {
            visit(*primary, 1, 1);
            visit(*excluded, 1, 1);
        }
        // Leaves — no outgoing edges. The propagation accumulates the
        // incident-edge sum into their frequency on the way in; no
        // further redistribution is needed.
        DtaState::Literal { .. }
        | DtaState::Regex { .. }
        | DtaState::Epsilon
        | DtaState::WsTrim { .. }
        | DtaState::ConsumeToNextStructural { .. } => {}
    }
}

/// Choose the visit multiplier for a `Repeat(lo, hi)` body.
///
/// - `(0, 1)` and `(1, 1)`: optional / exact-one — body visited at
///   most once, multiplier = 1.
/// - `(0, u32::MAX)` / `(1, u32::MAX)` and other unbounded shapes:
///   `REPEAT_BODY_MULTIPLIER` (≈ 4) — empirical mean iteration count.
/// - `(lo, hi)` with finite `hi`: `max(REPEAT_BODY_MULTIPLIER, hi)`.
fn repeat_multiplier(lo: u32, hi: u32) -> u32 {
    if hi <= 1 {
        1
    } else if hi == u32::MAX {
        REPEAT_BODY_MULTIPLIER
    } else {
        REPEAT_BODY_MULTIPLIER.max(hi).max(lo)
    }
}

// ── Diagnostics ──────────────────────────────────────────────────────

/// Produce a top-N slice of the state-visit frequency ordering.
///
/// Convenience over `compute_state_visit_frequency(table)
/// .into_iter().take(n).collect()` for emitter / test code that only
/// needs the hot prefix.
pub fn top_hot_states(table: &DtaTable, n: usize) -> Vec<(StateId, u32)> {
    let mut order = compute_state_visit_frequency(table);
    order.truncate(n);
    order
}

/// True when `table.states.len()` exceeds [`HOT_BUDGET`] — i.e., the
/// emitter (W4.b) should consult the frequency ordering and split the
/// state machine into hot + cold partitions. False keeps the whole
/// state machine inline in one function body.
#[inline]
pub fn requires_hot_cold_split(table: &DtaTable) -> bool {
    table.states.len() > HOT_BUDGET
}

/// Partition a state-visit ordering into (hot_set, cold_set) at
/// [`HOT_BUDGET`]. Returns two `HashSet<StateId>` so the emitter's
/// per-state arm dispatch can do an O(1) lookup. The hot set always
/// contains at least the entry state (provided the table is non-empty).
///
/// When `table.states.len() <= HOT_BUDGET`, every state lands in the
/// hot set and the cold set is empty — the emitter then skips the
/// `#[cold]` sibling generation entirely.
pub fn partition_hot_cold(
    table: &DtaTable,
    ordering: &[(StateId, u32)],
) -> (HashSet<StateId>, HashSet<StateId>) {
    let mut hot = HashSet::new();
    let mut cold = HashSet::new();
    let limit = if table.states.len() <= HOT_BUDGET {
        ordering.len()
    } else {
        HOT_BUDGET
    };
    for (idx, (state, _freq)) in ordering.iter().enumerate() {
        if idx < limit {
            hot.insert(*state);
        } else {
            cold.insert(*state);
        }
    }
    (hot, cold)
}

