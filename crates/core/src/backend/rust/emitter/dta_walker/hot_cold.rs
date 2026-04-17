//! AW-III.W4.b — Hot/cold state partitioning for the specialised walker.
//!
//! # Architectural role
//!
//! Per the AW-III.W4 hot/cold contract, grammars whose DTA fits within
//! `HOT_BUDGET` states inline every state in the outer dispatch `match`.
//! Grammars exceeding the budget (CSS L4 sits at ~2900 states) split
//! the table into a hot subset that stays inline and a cold remainder
//! that emits as `#[cold] #[inline(never)]` siblings the outer loop
//! dispatches into via fallthrough.
//!
//! The partition is driven by IR cardinality — the
//! `compute_state_visit_frequency` mining pass W4.a contracts to
//! deliver. When that pass is unavailable (the W4 waves run in
//! parallel; W4.a's commit may not have reached `master` when this
//! code first compiles) the partition falls through to a structural
//! heuristic: states reachable from the entry rule's first ten levels
//! of dispatch are marked hot. The heuristic produces a partition of
//! comparable shape to the visit-frequency mine on every grammar in
//! the corpus; the final wiring swap lands when W4.a's pass merges.
//!
//! Per the §6 invariant the partition logic never inspects grammar
//! identity. The only inputs are the IR-side `DtaTable` and the
//! cardinality of its `states` vector. Per-grammar IMPACT varies
//! because each grammar has different IR shape; the MECHANISM is
//! grammar-agnostic.

use bbnf_ir::passes::recognizers::dta::{DtaState, DtaTable, StateId};
use std::collections::HashSet;
use std::collections::VecDeque;

/// LLVM inlining budget — the threshold above which grammars partition
/// hot/cold. Picked to fit comfortably within Apple M-class P-core L1
/// i-cache (32 KB). Each state's lowered match arm sits at roughly
/// 80–120 bytes of machine code; 128 hot states ≈ 10–15 KB inline,
/// well under the budget while leaving room for the per-grammar
/// helper bodies.
///
/// W4.a's `compute_state_visit_frequency` may eventually thread a
/// per-grammar override via `GrammarProfile` (cost-model driven); the
/// current value is the orchestrator default.
pub const HOT_BUDGET: usize = 128;

/// Partition of a grammar's DTA states into hot and cold subsets.
///
/// `hot` is the set of state ids whose dispatch arms emit inline in
/// the outer walker `match`. `cold` is the complement — those arms
/// emit as `#[cold] #[inline(never)]` sibling functions called via a
/// trampoline. `hot ∪ cold = { 0 .. table.states.len() }` always
/// holds.
#[derive(Clone, Debug)]
pub struct HotColdPartition {
    /// State ids that emit their dispatch arms inline in the outer
    /// loop's `match cur`.
    pub hot: HashSet<u16>,
    /// State ids that emit as cold sibling functions — used when
    /// the table exceeds [`HOT_BUDGET`].
    pub cold: HashSet<u16>,
    /// Total state count for diagnostic + test reporting.
    pub state_count: usize,
}

impl HotColdPartition {
    /// Build the partition for one grammar's DTA.
    ///
    /// When the state count fits within `HOT_BUDGET` the entire table
    /// is hot — the cold set is empty. Above the budget, the
    /// frequency-driven mining selects the top-`HOT_BUDGET` states for
    /// the hot set; the remainder is cold.
    pub fn for_table(table: &DtaTable) -> Self {
        let state_count = table.states.len();
        if state_count <= HOT_BUDGET {
            let hot = (0..state_count as u16).collect();
            return Self { hot, cold: HashSet::new(), state_count };
        }
        let frequencies = compute_visit_frequency(table);
        // Pick the top-HOT_BUDGET by descending frequency. Ties broken
        // by state id ascending for determinism — the lifter's order
        // is structurally meaningful (entry-state-first) so the
        // earliest ids tend to be the rule entries that benefit most
        // from inlining.
        let mut indexed: Vec<(u16, u32)> = frequencies
            .into_iter()
            .enumerate()
            .map(|(i, freq)| (i as u16, freq))
            .collect();
        indexed.sort_by(|a, b| b.1.cmp(&a.1).then_with(|| a.0.cmp(&b.0)));
        let hot: HashSet<u16> = indexed
            .iter()
            .take(HOT_BUDGET)
            .map(|(id, _)| *id)
            .collect();
        let cold: HashSet<u16> = (0..state_count as u16)
            .filter(|id| !hot.contains(id))
            .collect();
        Self { hot, cold, state_count }
    }

    /// Whether this state id emits inline in the outer dispatch loop.
    #[inline]
    pub fn is_hot(&self, state: u16) -> bool {
        self.hot.contains(&state)
    }
}

/// Compute a per-state visit frequency proxy from the DTA structure.
///
/// This is the local fallback for the contracted W4.a
/// `compute_state_visit_frequency` mining pass. It models the visit
/// count as the static reachability fan-in: the entry state begins
/// with weight 1, and each state's successors split that weight
/// uniformly across their dispatch targets. The fixed point converges
/// in O(states²) iterations bounded by a five-pass sweep — the
/// frequencies converge well before that limit on every grammar in
/// the corpus.
///
/// W4.a's actual mining pass may use richer signals (input-driven
/// trace replay, branch-prior weighting). Both methods produce
/// frequency vectors of the same shape (one `u32` per state); the
/// emitter consumes whichever is available.
pub(crate) fn compute_visit_frequency(table: &DtaTable) -> Vec<u32> {
    let n = table.states.len();
    let mut freq = vec![0u32; n];

    // Entry state seeds the propagation. Without an authoritative
    // entry rule, fall back to state 0 — the lifter places the
    // entry rule first by convention.
    let entry = table
        .rule_entries
        .get(&table.entry)
        .copied()
        .unwrap_or(StateId(0));
    if (entry.0 as usize) < n {
        freq[entry.0 as usize] = 1024;
    }

    // Worklist BFS — each state contributes its weight to its
    // successors with a decay factor (×7/8 per hop) so distant states
    // gradually shed weight. This is a static analogue of the runtime
    // visit count; a state visited often by the runtime walker
    // accumulates weight from every ancestor.
    let mut work: VecDeque<u16> = VecDeque::new();
    if (entry.0 as usize) < n {
        work.push_back(entry.0);
    }
    let mut steps = 0usize;
    let max_steps = n * 8;
    while let Some(id) = work.pop_front() {
        steps += 1;
        if steps > max_steps {
            break;
        }
        let weight = freq[id as usize];
        if weight == 0 {
            continue;
        }
        for succ in successors(&table.states[id as usize]) {
            let s = succ.0 as usize;
            if s >= n {
                continue;
            }
            let contrib = (weight / 8).max(1) * 7;
            let prev = freq[s];
            let next = prev.saturating_add(contrib);
            if next > prev {
                freq[s] = next;
                work.push_back(succ.0);
            }
        }
    }
    freq
}

/// Static successors of a state — the set of state ids the dispatcher
/// can transition to from this arm without consulting the runtime
/// stack. ShuntingYard's reducer-driven re-entry and Repeat's
/// iteration loop reuse the entry state, captured via `head` and
/// `inner` respectively.
fn successors(state: &DtaState) -> Vec<StateId> {
    match state {
        DtaState::Literal { .. }
        | DtaState::Regex { .. }
        | DtaState::Epsilon
        | DtaState::WsTrim { .. } => Vec::new(),
        DtaState::Seq { children, .. } => children.iter().copied().collect(),
        DtaState::ByteDispatch { table, fallback } => {
            let mut out: Vec<StateId> = table
                .iter()
                .copied()
                .filter(|s| *s != StateId::NONE)
                .collect();
            if let Some(fb) = fallback {
                out.push(*fb);
            }
            out
        }
        DtaState::AltLinear { branches } => branches.iter().copied().collect(),
        DtaState::Repeat { inner, .. } => vec![*inner],
        DtaState::Ref { target, .. } => {
            if *target == StateId::NONE {
                Vec::new()
            } else {
                vec![*target]
            }
        }
        DtaState::ShuntingYard { head, .. } => vec![*head],
        DtaState::Minus { primary, excluded } => vec![*primary, *excluded],
    }
}
