//! AW-III.W5-carry / AW-IV.W3.5c — Per-pattern matchable-byte alphabet
//! miner + last-byte-set computation.
//!
//! # Why
//!
//! The W5.c bounded-Regex scan assumes every `IrNode::Regex` pattern
//! is bounded by the grammar's structural alphabet — "the next
//! structural byte is a hard match boundary". That assumption
//! collapses on dense-alphabet grammars: CSS L4 mines all printable
//! ASCII as structural, so the bound `[pos, idx.positions[slot])`
//! degenerates to `[pos, pos)` and every regex scan runs zero-width.
//!
//! # W3.5c invariant flip
//!
//! The pre-W3.5c admission test asked whether the pattern's full
//! matchable-byte alphabet was disjoint from the grammar's structural
//! alphabet. That is the *CTNS* invariant: if every byte the pattern
//! can match is non-structural, the walker may collapse the scan to a
//! cursor jump. Bounded-Regex — where the walker runs the pattern's
//! DFA but caps the scan at the next structural byte — is a strictly
//! weaker obligation: the pattern only has to *terminate* at the first
//! structural byte it encounters, not refuse to match structural bytes
//! entirely.
//!
//! The weaker obligation is encoded as the pattern's *last possible
//! byte set*: the set of bytes that can appear immediately before the
//! DFA reaches its accept state. Equivalently, the byte-transitions
//! whose target can ε-reach the NFA accept. If that set is disjoint
//! from the structural alphabet, the pattern always terminates before
//! a structural byte — the next structural position is a safe upper
//! bound on the DFA scan, because no accepting match can end on a
//! structural byte.
//!
//! CSS L4 identifiers (`[A-Za-z_][A-Za-z0-9_-]*`) fail the pre-W3.5c
//! admission (every alphanumeric byte is "structural" under the dense
//! CSS alphabet) but pass the W3.5c admission: the last byte of any
//! match is in `[A-Za-z0-9_-]`, and that set is disjoint from CSS's
//! structural delimiters `{};:,`. Similarly for Sheets formula
//! identifiers.
//!
//! # Output shape
//!
//! `MineOutputs::pattern_alphabets` — per-Regex-NodeId record carrying
//! both the full matchable-byte bitmap (CTNS admission data) and the
//! last-byte-set (bounded-Regex admission data). Empty when the
//! pattern's alphabet is unknown (pattern compilation failed / the
//! heuristic conservatively surrenders).

use crate::dag::NodeId;
use crate::passes::sets::structural_alphabet::{build_byte_bitmap, StructuralBitmap};
use crate::IrNode;

use super::{MineOutputs, RecognizerMineCtx, RecognizerMiner};

/// A mined per-pattern matchable-byte alphabet.
#[derive(Clone, Debug)]
pub struct PatternAlphabet {
    /// 256-bit bitmap of bytes the pattern's DFA can match. Packed
    /// as four 64-bit words.
    pub matchable_bytes: StructuralBitmap,
    /// AW-IV.W3.5c — the last-byte set: bytes that can appear
    /// immediately before the pattern's DFA enters its accept state.
    /// Computed from the NFA's byte-transitions whose target can
    /// ε-reach the accept state.
    ///
    /// The bounded-Regex admission test — a weaker obligation than
    /// `matchable_bytes` disjointness — checks that `last_byte_set` is
    /// disjoint from the grammar's structural alphabet. Conservative
    /// all-bytes answer when the NFA is unavailable; if the consumer
    /// sees `is_last_byte_tight == false` the safe action is to fall
    /// back to full-input scanning.
    pub last_byte_set: StructuralBitmap,
    /// True when mining succeeded and the bitmap is a strict
    /// upper-bound on the pattern's matchable set. False when mining
    /// fell through to the conservative "all bytes" answer.
    pub is_tight: bool,
    /// True when the `last_byte_set` bitmap was computed from the NFA
    /// and represents a strict upper-bound. False when mining fell
    /// through to the conservative all-bytes answer (NFA unavailable,
    /// accept state unreachable, etc.).
    pub is_last_byte_tight: bool,
}

/// Per-Regex-NodeId mined alphabet.
pub type PatternAlphabetMap = std::collections::HashMap<NodeId, PatternAlphabet>;

/// The pattern-alphabet mining pass.
pub struct PatternAlphabetMiner;

impl RecognizerMiner for PatternAlphabetMiner {
    fn inspect(
        &self,
        node: &IrNode,
        node_id: NodeId,
        ctx: &RecognizerMineCtx,
        outputs: &mut MineOutputs,
    ) {
        let IrNode::Regex(sid) = node else {
            return;
        };
        let pattern = &ctx.ir.strings[*sid as usize];
        if let Some(alphabet) = mine_pattern(pattern) {
            outputs.pattern_alphabets.insert(node_id, alphabet);
        }
    }
}

/// Mine both the matchable-byte alphabet and the last-byte set of a
/// regex pattern.
///
/// # Matchable bytes
///
/// Uses a lightweight heuristic scan of the pattern string: any byte
/// that appears outside of a regex metacharacter context is
/// matchable, plus any byte inside `[...]` bracket expressions minus
/// the bytes explicitly excluded by `[^...]`. Escaped characters and
/// ranges (`a-z`) expand to their literal byte sets. Patterns the
/// heuristic can't classify return `is_tight: false` with the
/// conservative full-byte bitmap.
///
/// # Last-byte set (AW-IV.W3.5c)
///
/// Delegates to [`compute_last_byte_set`], which builds the NFA via
/// `bbnf_regex::Nfa::from_pattern` and walks the byte-transitions whose
/// target can ε-reach the NFA accept state. The resulting bitmap is the
/// set of bytes that can end a match — the admission data for the
/// bounded-Regex walker arm. Patterns whose NFA construction fails
/// return `is_last_byte_tight: false` with the conservative full-byte
/// bitmap so the consumer falls back to full-input scanning.
fn mine_pattern(pattern: &str) -> Option<PatternAlphabet> {
    let matchable = mine_matchable_bytes(pattern);
    let (last_byte_set, is_last_byte_tight) = compute_last_byte_set(pattern);
    Some(PatternAlphabet {
        matchable_bytes: matchable.0,
        is_tight: matchable.1,
        last_byte_set,
        is_last_byte_tight,
    })
}

/// Mine the matchable-byte alphabet via the pattern-string heuristic.
///
/// Returns `(bitmap, is_tight)`. `is_tight == false` when the heuristic
/// conservatively falls back to the full-byte answer (unknown escape
/// class, `.`, malformed bracket, or empty alphabet).
fn mine_matchable_bytes(pattern: &str) -> (StructuralBitmap, bool) {
    let bytes = pattern.as_bytes();
    let mut matchable = [0u64; 4];
    let mut i = 0;
    let mut any_byte_seen = false;

    while i < bytes.len() {
        let b = bytes[i];
        match b {
            // Escape sequences.
            b'\\' if i + 1 < bytes.len() => {
                let esc = bytes[i + 1];
                match esc {
                    // Common escape classes — treat as "any byte"
                    // sentinels so the final bitmap is permissive.
                    b'd' | b'D' | b'w' | b'W' | b's' | b'S' | b'.' => {
                        return (full_bitmap(), false);
                    }
                    b'n' => add_byte(&mut matchable, b'\n'),
                    b't' => add_byte(&mut matchable, b'\t'),
                    b'r' => add_byte(&mut matchable, b'\r'),
                    c => add_byte(&mut matchable, c),
                }
                i += 2;
            }
            // Bracket expression `[abc]` or `[^abc]`.
            b'[' => {
                let end = bytes[i + 1..].iter().position(|&c| c == b']');
                match end {
                    Some(rel) => {
                        let inner = &bytes[i + 1..i + 1 + rel];
                        let negated = inner.first() == Some(&b'^');
                        let class_bytes = if negated { &inner[1..] } else { inner };
                        let mut class_set = [0u64; 4];
                        let mut j = 0;
                        while j < class_bytes.len() {
                            let c = class_bytes[j];
                            // Range `a-z`.
                            if j + 2 < class_bytes.len() && class_bytes[j + 1] == b'-' {
                                let lo = c;
                                let hi = class_bytes[j + 2];
                                for v in lo..=hi {
                                    add_byte(&mut class_set, v);
                                }
                                j += 3;
                                continue;
                            }
                            // Escape inside class.
                            if c == b'\\' && j + 1 < class_bytes.len() {
                                let esc = class_bytes[j + 1];
                                match esc {
                                    b'd' => {
                                        for v in b'0'..=b'9' {
                                            add_byte(&mut class_set, v);
                                        }
                                    }
                                    b'w' => {
                                        for v in b'0'..=b'9' {
                                            add_byte(&mut class_set, v);
                                        }
                                        for v in b'a'..=b'z' {
                                            add_byte(&mut class_set, v);
                                        }
                                        for v in b'A'..=b'Z' {
                                            add_byte(&mut class_set, v);
                                        }
                                        add_byte(&mut class_set, b'_');
                                    }
                                    b's' => {
                                        add_byte(&mut class_set, b' ');
                                        add_byte(&mut class_set, b'\t');
                                        add_byte(&mut class_set, b'\n');
                                        add_byte(&mut class_set, b'\r');
                                    }
                                    b'n' => add_byte(&mut class_set, b'\n'),
                                    b't' => add_byte(&mut class_set, b'\t'),
                                    b'r' => add_byte(&mut class_set, b'\r'),
                                    other => add_byte(&mut class_set, other),
                                }
                                j += 2;
                                continue;
                            }
                            add_byte(&mut class_set, c);
                            j += 1;
                        }
                        if negated {
                            // Complement over all 256 bytes.
                            for word_idx in 0..4 {
                                matchable[word_idx] |= !class_set[word_idx];
                            }
                            any_byte_seen = true;
                        } else {
                            for word_idx in 0..4 {
                                matchable[word_idx] |= class_set[word_idx];
                            }
                            any_byte_seen = true;
                        }
                        i += 2 + rel;
                    }
                    None => {
                        // Malformed — bail conservatively.
                        return (full_bitmap(), false);
                    }
                }
            }
            // `.` = any byte except newline.
            b'.' => {
                return (full_bitmap(), false);
            }
            // Metacharacters we don't enumerate — skip.
            b'*' | b'+' | b'?' | b'(' | b')' | b'|' | b'^' | b'$' | b'{' | b'}' => {
                i += 1;
            }
            // Literal byte.
            _ => {
                add_byte(&mut matchable, b);
                any_byte_seen = true;
                i += 1;
            }
        }
    }

    if any_byte_seen {
        (matchable, true)
    } else {
        // Empty alphabet → pattern matches nothing visible; fall
        // through to the conservative full-byte answer rather than
        // publish an empty bitmap the consumer would have to
        // special-case.
        (full_bitmap(), false)
    }
}

/// AW-IV.W3.5c — compute the pattern's last-byte set via the NFA.
///
/// Constructs the NFA via `bbnf_regex::Nfa::from_pattern` and walks the
/// byte-transitions whose target can ε-reach the NFA's accept state.
/// The union of those transitions' `ByteSet`s is the last-byte set: the
/// bytes that can appear immediately before the pattern accepts.
///
/// Returns `(bitmap, is_tight)`. `is_tight == false` when the NFA
/// construction fails (HIR parse error, unsupported features) so the
/// consumer falls back to the conservative full-byte answer.
fn compute_last_byte_set(pattern: &str) -> (StructuralBitmap, bool) {
    let Some(nfa) = bbnf_regex::Nfa::from_pattern(pattern) else {
        return (full_bitmap(), false);
    };

    // ε-closure-reverse from the accept state: states that can
    // ε-reach accept via 0+ ε-hops (the accept itself plus every
    // state with an ε-edge into a reachable state).
    //
    // Built as a worklist-driven fixpoint over the NFA's reversed
    // ε-adjacency; the NFAs emitted by this compiler stay well under
    // ~500 states per pattern, so the O(V + E) closure is cheap
    // relative to the DFA compile the walker would otherwise pay for.
    let n = nfa.states.len();
    let mut reaches_accept = vec![false; n];
    reaches_accept[nfa.accept as usize] = true;

    // Build reverse ε-adjacency so closure fixpoint is one pass.
    let mut rev_epsilon: Vec<Vec<u32>> = vec![Vec::new(); n];
    for (src, state) in nfa.states.iter().enumerate() {
        for edge in &state.epsilon {
            if (edge.target as usize) < n {
                rev_epsilon[edge.target as usize].push(src as u32);
            }
        }
    }

    // Worklist: states we've just confirmed reach accept; their
    // ε-predecessors become candidates.
    let mut worklist: Vec<u32> = vec![nfa.accept];
    while let Some(state_id) = worklist.pop() {
        for &pred in &rev_epsilon[state_id as usize] {
            let pred_idx = pred as usize;
            if !reaches_accept[pred_idx] {
                reaches_accept[pred_idx] = true;
                worklist.push(pred);
            }
        }
    }

    // Union the byte-transition sets whose target can ε-reach accept.
    let mut last_bytes = [0u64; 4];
    for state in &nfa.states {
        for (byte_set, target) in &state.transitions {
            let target_idx = *target as usize;
            if target_idx < n && reaches_accept[target_idx] {
                // ByteSet is packed [u64; 4]. `iter()` yields every
                // set byte; union into our StructuralBitmap via the
                // existing byte-level add helper.
                for b in byte_set.iter() {
                    add_byte(&mut last_bytes, b);
                }
            }
        }
    }

    // Defensive: if the NFA encodes a zero-width accept (empty
    // pattern, lookaround-only HIR) we have no incoming transitions
    // and the bitmap is empty — publish the conservative full-byte
    // answer so the consumer treats the admission as unknown.
    if last_bytes.iter().all(|&w| w == 0) {
        (full_bitmap(), false)
    } else {
        (last_bytes, true)
    }
}

#[inline]
fn add_byte(bitmap: &mut [u64; 4], b: u8) {
    let word = (b >> 6) as usize;
    let bit = b & 0x3F;
    bitmap[word] |= 1u64 << bit;
}

#[inline]
fn full_bitmap() -> [u64; 4] {
    [u64::MAX; 4]
}

#[inline]
#[allow(dead_code)]
fn bitmaps_disjoint(a: &StructuralBitmap, b: &StructuralBitmap) -> bool {
    (a[0] & b[0]) == 0
        && (a[1] & b[1]) == 0
        && (a[2] & b[2]) == 0
        && (a[3] & b[3]) == 0
}

/// Helper construction — used by tests and by the walker-side
/// runtime guard when consuming the mined alphabets.
///
/// The `matchable_bytes` and `last_byte_set` bitmaps both derive from
/// `bytes` — callers constructing fixtures against the pre-W3.5c API
/// get matchable == last_byte by default.
#[allow(dead_code)]
pub fn make_alphabet<I: IntoIterator<Item = u8>>(bytes: I) -> PatternAlphabet {
    let bitmap = build_byte_bitmap(bytes);
    PatternAlphabet {
        matchable_bytes: bitmap,
        is_tight: true,
        last_byte_set: bitmap,
        is_last_byte_tight: true,
    }
}
