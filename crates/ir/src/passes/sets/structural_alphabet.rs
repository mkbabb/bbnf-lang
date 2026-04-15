//! `compute_structural_alphabet` — derives per-grammar structural
//! alphabets for the scanner-kernel emitters.
//!
//! A "structural byte" is any byte value that could terminate a
//! scanner's inner loop — an Alt-dispatch starter, a terminal literal
//! starter, or the first byte of a structural digraph (`/*`, `*/`,
//! `->`, `(*`, `*)`). When the set size is small (≤ 8, the nibble-LUT
//! window), codegen in `crates/core/src/generate/regex/emit/simd.rs`
//! can emit a grammar-wide bitmap kernel that every scan site routes
//! through.
//!
//! ## Archaeology (AO.0.1 / AU.2.7 v2)
//!
//! This pass revives the data-flow shape of `compute_structural_bytes`
//! (commit `4114695`, deleted at commit `2f7c1bd`). What failed in v1
//! was integration — a `structural_cursor` on `ParserState`, a scalar
//! `filter_quote_parity`, hybrid dispatch in `alt.rs`. v2 keeps the
//! derivation strategy and discards all of that: the alphabet is read
//! by the codegen-time emitter, not by a runtime cursor. The set is
//! grammar-parameterised; the bitmap is scanner-local.
//!
//! ## Digraph set
//!
//! Known digraphs across the four grammar families:
//!
//! | digraph | grammar | role |
//! |---------|---------|------|
//! | `/\*`   | CSS, BBNF | block comment opener |
//! | `\*/`   | CSS, BBNF | block comment closer |
//! | `->`    | BBNF | type-annotation arrow |
//! | `(\*`   | BBNF (optional) | EBNF-style opener |
//! | `\*)`   | BBNF (optional) | EBNF-style closer |
//!
//! The digraph table is statically small; we enumerate the known
//! forms and filter by which first-byte appears in `S`.

use crate::{GrammarIR, IrNode};

use serde::{Deserialize, Serialize};

use std::collections::BTreeSet;

/// Structural alphabet `(S, D)` for a grammar.
#[derive(Serialize, Deserialize, Clone, Debug, Default)]
pub struct StructuralAlphabet {
    /// Single-byte structural set. Bytes that can terminate a
    /// scanner's inner loop.
    pub single_bytes: BTreeSet<u8>,

    /// Two-byte digraphs observable at scanner boundaries. First
    /// byte is always in `single_bytes`.
    pub digraphs: Vec<(u8, u8)>,
}

impl StructuralAlphabet {
    /// Return the alphabet as a sorted Vec<u8> — convenient for
    /// passing into nibble-LUT codegen.
    pub fn single_bytes_vec(&self) -> Vec<u8> {
        self.single_bytes.iter().copied().collect()
    }

    /// Return `true` iff `bytes` is a strict subset of `S`. Used by
    /// emitters to short-circuit per-site LUT construction when the
    /// grammar-wide LUT already covers the site.
    pub fn covers(&self, bytes: &[u8]) -> bool {
        bytes.iter().all(|b| self.single_bytes.contains(b))
    }
}

/// Walk every rule body, collect dispatch-table bytes, literal
/// first-bytes, and digraph first-bytes. Store the result as
/// `ir.structural_alphabet` whenever the set size is within the
/// nibble-LUT window (2..=8 unique bytes).
pub fn compute_structural_alphabet(ir: &mut GrammarIR) {
    let mut byte_set: BTreeSet<u8> = BTreeSet::new();

    for rule in &ir.rules {
        collect_bytes(&rule.body, ir, &mut byte_set);
    }

    // Digraph enumeration: produce all pairs whose first byte is
    // structural. Callers filter down to the ones actually observed
    // in the grammar by checking rule literals. For v2 we keep the
    // table small — CSS+BBNF care about `/*`, `*/`, `->`, `(*`,
    // `*)`. The first byte of every digraph must be in `S`; if it
    // is not, the digraph is irrelevant (no scanner path will check
    // for it).
    let mut digraphs: Vec<(u8, u8)> = Vec::new();
    let candidate_digraphs: [(u8, u8); 5] = [
        (b'/', b'*'),
        (b'*', b'/'),
        (b'-', b'>'),
        (b'(', b'*'),
        (b'*', b')'),
    ];
    for (first, second) in candidate_digraphs {
        if byte_set.contains(&first) && observes_digraph(ir, first, second) {
            digraphs.push((first, second));
            // A digraph whose first byte isn't already in S could
            // still widen the alphabet — e.g. `->` in BBNF adds `-`
            // if the grammar never uses `-` as a single-byte
            // terminator. Add the first byte so the bitmap kernel
            // sees every digraph opener.
            byte_set.insert(first);
        }
    }

    // Gate: store only when the set is within the nibble-LUT window.
    // A grammar with 9+ unique structural bytes falls back to the
    // per-site emitter path (which still uses the same
    // find_next_structural_from helper but with a smaller per-site
    // LUT).
    if byte_set.len() >= 2 && byte_set.len() <= 8 {
        ir.structural_alphabet = Some(StructuralAlphabet {
            single_bytes: byte_set,
            digraphs,
        });
    }
}

fn observes_digraph(ir: &GrammarIR, first: u8, second: u8) -> bool {
    // A digraph is "observed" iff any interned string contains it
    // as a literal. This is a proxy for the grammar actually caring
    // about the two-byte sequence.
    let pat: [u8; 2] = [first, second];
    ir.strings.iter().any(|s| s.as_bytes().windows(2).any(|w| w == pat))
}

fn collect_bytes(node: &IrNode, ir: &GrammarIR, bytes: &mut BTreeSet<u8>) {
    match node {
        IrNode::Alt(branches, dispatch_opt) => {
            if let Some(dispatch) = dispatch_opt {
                for (byte_val, &branch_idx) in dispatch.table.iter().enumerate() {
                    if branch_idx != 0xFF {
                        bytes.insert(byte_val as u8);
                    }
                }
            }
            for b in branches {
                collect_bytes(&b.node, ir, bytes);
            }
        }
        IrNode::Literal(sid) => {
            if let Some(first) = ir.strings[*sid as usize].as_bytes().first() {
                bytes.insert(*first);
            }
        }
        IrNode::Seq(children) => {
            for c in children {
                collect_bytes(c, ir, bytes);
            }
        }
        IrNode::Repeat { inner, .. }
        | IrNode::OptionalWhitespace(inner)
        | IrNode::Negate(inner)
        | IrNode::Map { inner, .. } => collect_bytes(inner, ir, bytes),
        IrNode::Skip(a, b) | IrNode::Next(a, b) | IrNode::Minus(a, b) => {
            collect_bytes(a, ir, bytes);
            collect_bytes(b, ir, bytes);
        }
        IrNode::TokenDispatch {
            token,
            arms,
            fallback,
        } => {
            collect_bytes(token, ir, bytes);
            for arm in arms {
                collect_bytes(&arm.continuation, ir, bytes);
            }
            collect_bytes(fallback, ir, bytes);
        }
        IrNode::Regex(_) | IrNode::Epsilon | IrNode::Ref(_) => {}
    }
}

// Tests live in tests/structural_alphabet.rs (crate-level).
