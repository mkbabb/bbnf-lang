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
//! ## Archaeology (AO.0.1 / AU.2.7 v2 / AW-III.W5.a)
//!
//! AW-III.W5.a (this file) extends the AU.2.7 v2 derivation with three
//! new fields the stage-1 SIMD pre-pass kernel consumes per the
//! `bbnf-simd-scan` crate's alphabet contract:
//!
//! - `digraph_mask: [u64; 4]` — 256-bit bitmap of digraph **first**
//!   bytes. Lets the kernel cheaply ask "is this byte a potential
//!   digraph opener" without scanning a `Vec`.
//! - `digraph_pairs: Vec<(u8, u8)>` — full mined digraph pair list.
//!   Generalised from the AU.2.7 candidate-table approach: the v2
//!   pass hardcoded a 5-pair candidate list and asked which were
//!   "observed"; v3 walks every `IrNode::Literal` and harvests the
//!   first two bytes of any 2+ byte literal whose first byte appears
//!   in the structural set. No grammar-name conditionals; the data is
//!   the data.
//! - `quote_classes: BTreeSet<u8>` — bytes that toggle string mode.
//!   Sourced from `IrNode::Regex` nodes whose `regex_info`
//!   classification is `RegexClass::QuotedString { quote_char, .. }`.
//!   The CLMUL/PMULL parity path in the kernel needs this set so it
//!   can mask off bytes inside string literals before compaction.
//!
//! ## Digraph derivation
//!
//! Pre-W5.a the digraph table was a hardcoded candidate list filtered
//! by literal occurrence. W5.a removes the candidate list entirely:
//! every 2+ byte literal in the grammar contributes its first two
//! bytes as a candidate digraph; the pair survives if its first byte
//! is also in the structural single-byte set. This is fully general
//! across grammar families — no list maintenance, no per-grammar
//! special cases.

use crate::{GrammarIR, IrNode, StringId};

use serde::{Deserialize, Serialize};

use std::collections::BTreeSet;

/// Width of the [`StructuralAlphabet`] bitmap fields, in `u64` words.
/// Four 64-bit words cover the full 256-byte universe (`u8` value
/// space) — the same shape `bbnf-simd-scan` consumes.
pub const STRUCTURAL_BITMAP_WORDS: usize = 4;

/// 256-bit bitmap of `u8` values, packed as four 64-bit words. Word
/// `i` covers bytes `64*i .. 64*(i+1)`. Stable wire shape; the SIMD
/// kernel reads it directly.
pub type StructuralBitmap = [u64; STRUCTURAL_BITMAP_WORDS];

/// Construct a [`StructuralBitmap`] from an iterator of `u8` values.
/// Idempotent under repeated bytes.
pub fn build_byte_bitmap<I: IntoIterator<Item = u8>>(bytes: I) -> StructuralBitmap {
    let mut bitmap = [0u64; STRUCTURAL_BITMAP_WORDS];
    for byte in bytes {
        let word = (byte >> 6) as usize;
        let bit = byte & 0x3F;
        bitmap[word] |= 1u64 << bit;
    }
    bitmap
}

/// Test whether a byte is set in a [`StructuralBitmap`].
#[inline]
pub fn bitmap_contains(bitmap: &StructuralBitmap, byte: u8) -> bool {
    let word = (byte >> 6) as usize;
    let bit = byte & 0x3F;
    (bitmap[word] >> bit) & 1 == 1
}

/// Total set bits in a [`StructuralBitmap`].
#[inline]
pub fn bitmap_popcount(bitmap: &StructuralBitmap) -> u32 {
    bitmap.iter().map(|w| w.count_ones()).sum()
}

/// Structural alphabet `(S, D, Q)` for a grammar.
///
/// `S` is `single_bytes` — bytes a scanner inner-loop may legally
/// terminate on. `D` is the digraph set (`digraph_pairs` plus the
/// `digraph_mask` first-byte bitmap). `Q` is `quote_classes` — bytes
/// that toggle string mode and therefore demand a parity correction
/// pass before the kernel emits its compacted index.
#[derive(Serialize, Deserialize, Clone, Debug, Default)]
pub struct StructuralAlphabet {
    /// Single-byte structural set. Bytes that can terminate a
    /// scanner's inner loop.
    pub single_bytes: BTreeSet<u8>,

    /// Two-byte digraphs observable at scanner boundaries. First
    /// byte is always in `single_bytes`.
    pub digraphs: Vec<(u8, u8)>,

    /// Bitmap of digraph first-bytes. Equivalent to
    /// `digraphs.iter().map(|(a, _)| *a).collect()` packed into a
    /// 256-bit bitmap; pre-computed so the SIMD kernel can mask
    /// candidate-opener lanes in one ANDS without a derefenced loop.
    #[serde(default)]
    pub digraph_mask: StructuralBitmap,

    /// String-toggle byte set. Mined from `IrNode::Regex` nodes
    /// classified as `RegexClass::QuotedString`. Drives the CLMUL /
    /// PMULL / shift-XOR quote-parity path in `bbnf-simd-scan`.
    #[serde(default)]
    pub quote_classes: BTreeSet<u8>,
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

    /// Return the quote-class set as a sorted `Vec<u8>`. Convenience
    /// for the kernel-shape selector and the wire emitter.
    pub fn quote_classes_vec(&self) -> Vec<u8> {
        self.quote_classes.iter().copied().collect()
    }

    /// 256-bit bitmap of `single_bytes`, packed for SIMD consumption.
    /// Computed on demand from the BTreeSet; no field cache.
    pub fn singletons_mask(&self) -> StructuralBitmap {
        build_byte_bitmap(self.single_bytes.iter().copied())
    }

    /// 256-bit bitmap of `quote_classes`, packed for SIMD consumption.
    pub fn quote_classes_mask(&self) -> StructuralBitmap {
        build_byte_bitmap(self.quote_classes.iter().copied())
    }
}

/// Walk every rule body, collect dispatch-table bytes, literal
/// first-bytes, digraph first-bytes, and string-toggle bytes. Store
/// the result on `ir.structural_alphabet` whenever the singleton set
/// is within the nibble-LUT window (2..=8 unique bytes); larger
/// alphabets still surface, with the kernel-shape selector picking
/// `WideLut` or `MultipassCmpEq` per cardinality.
pub fn compute_structural_alphabet(ir: &mut GrammarIR) {
    let mut byte_set: BTreeSet<u8> = BTreeSet::new();

    for rule in &ir.rules {
        collect_bytes(&rule.body, ir, &mut byte_set);
    }

    // Digraph mining: harvest every 2+ byte literal in the grammar.
    // No hardcoded candidate list — the data is the data. A digraph
    // pair `(a, b)` is admitted iff `a` is in the structural set
    // (otherwise no scanner path will ever check for it). Mining is
    // deduplicated and sorted for determinism.
    let mut digraph_set: BTreeSet<(u8, u8)> = BTreeSet::new();
    for sid in 0..ir.strings.len() {
        let bytes = ir.strings[sid].as_bytes();
        if bytes.len() < 2 {
            continue;
        }
        // Walk every 2-byte window in the literal — a 3-byte literal
        // like `==!` has digraph candidates `==` and `=!`. Each
        // contributes if its first byte is structural.
        for window in bytes.windows(2) {
            let first = window[0];
            let second = window[1];
            if byte_set.contains(&first) {
                digraph_set.insert((first, second));
            }
        }
    }

    // Add digraph first-bytes to the structural set so the bitmap
    // kernel sees every digraph opener even if the byte was not
    // already a single-byte terminator (e.g. `-` in BBNF's `->` arrow
    // when no rule uses `-` as a leaf alone).
    for (first, _) in &digraph_set {
        byte_set.insert(*first);
    }

    let digraphs: Vec<(u8, u8)> = digraph_set.iter().copied().collect();
    let digraph_mask = build_byte_bitmap(digraphs.iter().map(|(a, _)| *a));

    // Quote-class mining: walk `IrNode::Regex` nodes; for each, look
    // up `regex_info` and admit the `quote_char` of any pattern
    // classified as `RegexClass::QuotedString`. The classifier is the
    // single source of truth; no string-pattern matching here.
    let mut quote_classes: BTreeSet<u8> = BTreeSet::new();
    let mut regex_sids: BTreeSet<StringId> = BTreeSet::new();
    for rule in &ir.rules {
        collect_regex_sids(&rule.body, &mut regex_sids);
    }
    for sid in regex_sids {
        if let Some(info) = ir.regex_info.get(&sid) {
            if let bbnf_regex::RegexClass::QuotedString { quote_char, .. } = info.classification {
                quote_classes.insert(quote_char);
            }
        }
    }

    // Always store the alphabet — the kernel-shape selector picks the
    // appropriate codegen per cardinality. Pre-W5.a only the
    // ≤8-singleton window stored; W5.a relaxes that gate so wide
    // alphabets and quote-only grammars also flow through the bitmap
    // kernel pipeline.
    if !byte_set.is_empty() || !quote_classes.is_empty() {
        ir.structural_alphabet = Some(StructuralAlphabet {
            single_bytes: byte_set,
            digraphs,
            digraph_mask,
            quote_classes,
        });
    }
}

/// Recursively collect every interned regex `StringId` referenced from
/// a node tree. Used to look up `RegexInfo::classification` without
/// re-walking the IR per-node.
fn collect_regex_sids(node: &IrNode, sids: &mut BTreeSet<StringId>) {
    match node {
        IrNode::Regex(sid) => {
            sids.insert(*sid);
        }
        IrNode::Alt(branches, _) => {
            for b in branches {
                collect_regex_sids(&b.node, sids);
            }
        }
        IrNode::Seq(children) => {
            for c in children {
                collect_regex_sids(c, sids);
            }
        }
        IrNode::Repeat { inner, .. }
        | IrNode::OptionalWhitespace(inner)
        | IrNode::Negate(inner)
        | IrNode::Map { inner, .. } => collect_regex_sids(inner, sids),
        IrNode::Skip(a, b) | IrNode::Next(a, b) | IrNode::Minus(a, b) => {
            collect_regex_sids(a, sids);
            collect_regex_sids(b, sids);
        }
        IrNode::TokenDispatch {
            token,
            arms,
            fallback,
        } => {
            collect_regex_sids(token, sids);
            for arm in arms {
                collect_regex_sids(&arm.continuation, sids);
            }
            collect_regex_sids(fallback, sids);
        }
        IrNode::Literal(_) | IrNode::Epsilon | IrNode::Ref(_) => {}
    }
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

// Tests live in tests/structural_alphabet_extended.rs (crate-level).
