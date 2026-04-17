//! AW-III.W5-carry — Per-pattern matchable-byte alphabet miner.
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
//! The fix is per-pattern alphabet mining: for each regex pattern,
//! compute the *tight* set of bytes the pattern's DFA can match. If
//! that set is disjoint from the grammar's structural alphabet, the
//! bounded scan is safe; otherwise the walker falls back to full-
//! input scanning.
//!
//! # Output shape
//!
//! `MineOutputs::pattern_alphabets` — per-Regex-NodeId 256-bit bitmap
//! of the matchable bytes. Empty when the pattern's alphabet is
//! unknown (pattern compilation failed / conservatively surrenders).
//!
//! The walker's Regex arm consults the mined bitmap: if the pattern's
//! bitmap is disjoint from the grammar's structural-byte bitmap, the
//! bounded scan is sound and the `[pos, idx.positions[slot])` bound
//! applies. Otherwise fall back to full-input scanning so the scan
//! doesn't truncate mid-match.

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
    /// True when mining succeeded and the bitmap is a strict
    /// upper-bound on the pattern's matchable set. False when mining
    /// fell through to the conservative "all bytes" answer.
    pub is_tight: bool,
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

/// Mine the matchable-byte alphabet of a regex pattern.
///
/// Uses a lightweight heuristic scan of the pattern string: any byte
/// that appears outside of a regex metacharacter context is
/// matchable, plus any byte inside `[...]` bracket expressions minus
/// the bytes explicitly excluded by `[^...]`. Escaped characters and
/// ranges (`a-z`) expand to their literal byte sets.
///
/// This is deliberately a heuristic — a full regex analyser would
/// require the DFA, which `parse-that`'s `Dfa` builder provides but
/// at compile-time cost we don't want to pay per-mine. The heuristic
/// gives tight alphabets for the grammar-authored patterns (JSON
/// numbers, CSS idents, BBNF quoted strings); patterns that defeat
/// the heuristic surface as `is_tight: false` and the consumer
/// treats them as "unknown" (safe fallback to full-scan).
fn mine_pattern(pattern: &str) -> Option<PatternAlphabet> {
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
                        return Some(PatternAlphabet {
                            matchable_bytes: full_bitmap(),
                            is_tight: false,
                        });
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
                        return Some(PatternAlphabet {
                            matchable_bytes: full_bitmap(),
                            is_tight: false,
                        });
                    }
                }
            }
            // `.` = any byte except newline.
            b'.' => {
                return Some(PatternAlphabet {
                    matchable_bytes: full_bitmap(),
                    is_tight: false,
                });
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
        Some(PatternAlphabet {
            matchable_bytes: matchable,
            is_tight: true,
        })
    } else {
        // Empty alphabet → pattern matches nothing visible; fall
        // through to the conservative full-byte answer rather than
        // publish an empty bitmap the consumer would have to
        // special-case.
        Some(PatternAlphabet {
            matchable_bytes: full_bitmap(),
            is_tight: false,
        })
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
#[allow(dead_code)]
pub fn make_alphabet<I: IntoIterator<Item = u8>>(bytes: I) -> PatternAlphabet {
    PatternAlphabet {
        matchable_bytes: build_byte_bitmap(bytes),
        is_tight: true,
    }
}
