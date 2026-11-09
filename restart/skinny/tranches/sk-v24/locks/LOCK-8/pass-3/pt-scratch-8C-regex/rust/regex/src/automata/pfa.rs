//! Priority-preserving DFA — leftmost-first (Perl/PCRE/JS `RegExp`) semantics.
//!
//! LOCK-8 · pass-3 · family 8C · PROTO-2 (Route A probe).
//!
//! The existing [`super::dfa::Dfa`] discards ε-edge priority: subset
//! construction stores an unordered `BTreeSet<StateId>` and `find_at`
//! returns leftmost-**longest**. This module is the Route-A counter-artifact:
//! an *ordered* ε-closure with a **match-cut** that preserves the priority the
//! NFA already carries (`EpsilonEdge::priority`), yielding leftmost-**first**.
//!
//! The whole of the difference lives in construction — `find_at` is byte-for-
//! byte identical to the longest engine. Under `MatchKind::LeftmostFirst` the
//! match-cut prunes every lower-priority consumer thread, so the very states
//! that the longest engine keeps alive to chase a longer match are simply
//! `DEAD` here, and the same forward walk stops at the first-alternative match.

use std::collections::HashMap;

use super::nfa::{DEAD, Nfa, StateId};
use crate::sets::equiv::compute_byte_classes;

/// Which match discipline the priority engine realizes.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MatchKind {
    /// Legacy: leftmost-longest (priority discarded). Provided for parity checks.
    LeftmostLongest,
    /// Route A: leftmost-first — the first alternative / lazy-shortest / greedy-
    /// longest discipline of Perl, PCRE and ECMAScript `RegExp`.
    LeftmostFirst,
}

/// A single priority-DFA state.
#[derive(Clone, Debug)]
pub struct PState {
    /// Transitions indexed by byte equivalence class (`DEAD` = none).
    pub transitions: Vec<StateId>,
    /// Whether the ordered thread list contains the NFA accept state.
    pub is_accept: bool,
}

/// A compiled priority DFA.
#[derive(Clone, Debug)]
pub struct Pdfa {
    pub states: Vec<PState>,
    pub byte_classes: [u8; 256],
    pub num_classes: u16,
    pub kind: MatchKind,
}

impl Pdfa {
    /// Compile a pattern to a priority DFA under `kind`.
    pub fn compile(pattern: &str, kind: MatchKind) -> Option<Self> {
        let nfa = Nfa::from_pattern(pattern)?;
        Self::from_nfa(&nfa, kind, true)
    }

    /// Build from a pre-constructed NFA. `minimize` runs the same Hopcroft
    /// partition-refinement the longest engine uses (R2 probe).
    pub fn from_nfa(nfa: &Nfa, kind: MatchKind, minimize: bool) -> Option<Self> {
        let byte_sets = nfa.transition_byte_sets();
        let (byte_classes, num_classes) = compute_byte_classes(&byte_sets);
        let class_reps = class_representatives(&byte_classes, num_classes);

        let mut pdfa = subset_construction(nfa, kind, &byte_classes, num_classes, &class_reps)?;
        if minimize && pdfa.states.len() > 1 {
            pdfa = hopcroft(&pdfa);
        }
        Some(pdfa)
    }

    pub fn state_count(&self) -> usize {
        self.states.len()
    }

    /// Anchored match at `offset`. Identical forward walk for both kinds — the
    /// discipline is baked into the transition table by construction.
    pub fn find_at(&self, bytes: &[u8], offset: usize) -> Option<usize> {
        let mut state: u32 = 0;
        let mut pos = offset;
        let mut last_accept = if self.states[0].is_accept {
            Some(pos)
        } else {
            None
        };
        while pos < bytes.len() {
            let class = self.byte_classes[bytes[pos] as usize] as usize;
            let next = self.states[state as usize].transitions[class];
            if next == DEAD {
                break;
            }
            state = next;
            pos += 1;
            if self.states[state as usize].is_accept {
                last_accept = Some(pos);
            }
        }
        last_accept
    }
}

// ── Ordered ε-closure with match-cut ──────────────────────────────────────

/// Compute the priority-ordered ε-closure of `seeds`.
///
/// Returns the ordered, deduped list of *thread heads* — states that consume a
/// byte, plus (possibly) the accept state — in strict priority order.
///
/// The **match-cut**: the moment the accept state is reached, the entire
/// traversal halts. Every thread that would be appended afterwards is lower
/// priority than an available match and is discarded — this, and only this, is
/// what turns leftmost-longest into leftmost-first. Under `LeftmostLongest` the
/// cut is disabled, so accept is recorded but siblings continue (parity mode).
fn ordered_closure(nfa: &Nfa, seeds: &[StateId], kind: MatchKind) -> (Vec<StateId>, bool) {
    let mut order: Vec<StateId> = Vec::new();
    let mut seen = vec![false; nfa.states.len()];
    let mut is_accept = false;
    for &s in seeds {
        if visit(nfa, s, kind, &mut seen, &mut order, &mut is_accept) {
            break; // cut propagated to the top level
        }
    }
    (order, is_accept)
}

/// Priority-ordered pre-order DFS. Returns `true` when a match-cut fired and the
/// caller must abandon its remaining (lower-priority) siblings.
fn visit(
    nfa: &Nfa,
    s: StateId,
    kind: MatchKind,
    seen: &mut [bool],
    order: &mut Vec<StateId>,
    is_accept: &mut bool,
) -> bool {
    if seen[s as usize] {
        return false;
    }
    seen[s as usize] = true;

    if s == nfa.accept {
        *is_accept = true;
        order.push(s);
        // Match-cut only under leftmost-first.
        return matches!(kind, MatchKind::LeftmostFirst);
    }

    let st = &nfa.states[s as usize];
    if !st.transitions.is_empty() {
        // A byte-consuming thread head — a leaf for closure purposes.
        order.push(s);
        return false;
    }

    // Routing node: descend ε-edges in priority order (lower value preferred).
    let mut edges: Vec<(u8, StateId)> = st.epsilon.iter().map(|e| (e.priority, e.target)).collect();
    edges.sort_by_key(|&(p, _)| p);
    for (_, target) in edges {
        if visit(nfa, target, kind, seen, order, is_accept) {
            return true; // propagate cut
        }
    }
    false
}

/// Step every consumer thread in `order` on representative byte `rep`, preserving
/// priority order.
fn move_on(nfa: &Nfa, order: &[StateId], rep: u8) -> Vec<StateId> {
    let mut out = Vec::new();
    for &s in order {
        for (bs, target) in &nfa.states[s as usize].transitions {
            if bs.contains(rep) {
                out.push(*target);
            }
        }
    }
    out
}

// ── Subset construction over ORDERED closures ─────────────────────────────

fn subset_construction(
    nfa: &Nfa,
    kind: MatchKind,
    byte_classes: &[u8; 256],
    num_classes: u16,
    class_reps: &[u8],
) -> Option<Pdfa> {
    let num_cls = num_classes as usize;
    let (start_order, start_acc) = ordered_closure(nfa, &[nfa.start], kind);

    let mut states: Vec<PState> = Vec::new();
    // Key is the ORDERED closure — [A,B] ≠ [B,A]. This is the crux of Route A.
    let mut key_to_id: HashMap<Vec<StateId>, u32> = HashMap::new();
    let mut worklist: Vec<(Vec<StateId>, u32)> = Vec::new();

    states.push(PState {
        transitions: vec![DEAD; num_cls],
        is_accept: start_acc,
    });
    key_to_id.insert(start_order.clone(), 0);
    worklist.push((start_order, 0));

    const STATE_LIMIT: usize = 4096;

    while let Some((cur_order, cur_id)) = worklist.pop() {
        for cls in 0..num_cls {
            let rep = class_reps[cls];
            let moved = move_on(nfa, &cur_order, rep);
            if moved.is_empty() {
                continue;
            }
            let (next_order, next_acc) = ordered_closure(nfa, &moved, kind);
            if next_order.is_empty() {
                continue;
            }
            let next_id = if let Some(&id) = key_to_id.get(&next_order) {
                id
            } else {
                if states.len() >= STATE_LIMIT {
                    return None;
                }
                let id = states.len() as u32;
                states.push(PState {
                    transitions: vec![DEAD; num_cls],
                    is_accept: next_acc,
                });
                key_to_id.insert(next_order.clone(), id);
                worklist.push((next_order, id));
                id
            };
            states[cur_id as usize].transitions[cls] = next_id;
        }
    }

    Some(Pdfa {
        states,
        byte_classes: *byte_classes,
        num_classes,
        kind,
    })
}

fn class_representatives(byte_classes: &[u8; 256], num_classes: u16) -> Vec<u8> {
    let mut reps = vec![0u8; num_classes as usize];
    let mut found = vec![false; num_classes as usize];
    for (b, &cls) in byte_classes.iter().enumerate() {
        if !found[cls as usize] {
            reps[cls as usize] = b as u8;
            found[cls as usize] = true;
        }
    }
    reps
}

// ── Hopcroft minimization (R2 probe) ──────────────────────────────────────
//
// Same partition-refinement as the longest engine; operating on a priority DFA
// it merges only states with identical accept-status *and* identical transition
// behaviour, so the match-cut already encoded in the transitions is preserved.

fn hopcroft(pdfa: &Pdfa) -> Pdfa {
    let n = pdfa.states.len();
    let num_cls = pdfa.num_classes as usize;
    let mut partition: Vec<usize> = vec![0; n];
    let mut num_blocks = 1;

    let has_a = pdfa.states.iter().any(|s| s.is_accept);
    let has_na = pdfa.states.iter().any(|s| !s.is_accept);
    if has_a && has_na {
        num_blocks = 2;
        for (i, s) in pdfa.states.iter().enumerate() {
            partition[i] = if s.is_accept { 0 } else { 1 };
        }
    }

    let mut changed = true;
    while changed {
        changed = false;
        for block_id in 0..num_blocks {
            let members: Vec<usize> = (0..n).filter(|&s| partition[s] == block_id).collect();
            if members.len() <= 1 {
                continue;
            }
            for cls in 0..num_cls {
                let first = members[0];
                let ft = pdfa.states[first].transitions[cls];
                let fb = if ft == DEAD {
                    usize::MAX
                } else {
                    partition[ft as usize]
                };
                let mut splitters = Vec::new();
                for &m in &members[1..] {
                    let t = pdfa.states[m].transitions[cls];
                    let tb = if t == DEAD {
                        usize::MAX
                    } else {
                        partition[t as usize]
                    };
                    if tb != fb {
                        splitters.push(m);
                    }
                }
                if !splitters.is_empty() {
                    let nb = num_blocks;
                    num_blocks += 1;
                    for &s in &splitters {
                        partition[s] = nb;
                    }
                    changed = true;
                    break;
                }
            }
            if changed {
                break;
            }
        }
    }

    let mut block_rep: Vec<Option<usize>> = vec![None; num_blocks];
    for (s, &b) in partition.iter().enumerate() {
        if block_rep[b].is_none() {
            block_rep[b] = Some(s);
        }
    }
    let mut block_to_new = vec![DEAD; num_blocks];
    let start_block = partition[0];
    block_to_new[start_block] = 0;
    let mut new_states = vec![PState {
        transitions: vec![DEAD; num_cls],
        is_accept: pdfa.states[block_rep[start_block].unwrap()].is_accept,
    }];
    let mut next_id: u32 = 1;
    for block in 0..num_blocks {
        if block_to_new[block] != DEAD {
            continue;
        }
        if let Some(rep) = block_rep[block] {
            block_to_new[block] = next_id;
            new_states.push(PState {
                transitions: vec![DEAD; num_cls],
                is_accept: pdfa.states[rep].is_accept,
            });
            next_id += 1;
        }
    }
    for block in 0..num_blocks {
        let Some(rep) = block_rep[block] else { continue };
        let new_id = block_to_new[block];
        if new_id == DEAD {
            continue;
        }
        for cls in 0..num_cls {
            let ot = pdfa.states[rep].transitions[cls];
            new_states[new_id as usize].transitions[cls] = if ot == DEAD {
                DEAD
            } else {
                block_to_new[partition[ot as usize]]
            };
        }
    }

    Pdfa {
        states: new_states,
        byte_classes: pdfa.byte_classes,
        num_classes: pdfa.num_classes,
        kind: pdfa.kind,
    }
}
