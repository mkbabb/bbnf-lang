//! `compute_structural_alphabet` — derives per-grammar structural
//! alphabets for the scanner-kernel emitters.
//!
//! A "structural byte" is any byte value that **delineates parse-tree
//! structure** — the simdjson definition, not "any byte appearing in
//! any rule's FIRST set". Concretely, a byte is structural iff:
//!
//! 1. It terminates a single-byte `Literal` (`{`, `}`, `;`, `:`, `(`,
//!    `)`, `[`, `]`, `,`, `<`, `>`, `=`, `!`, `?`, `*`, `+`, `-`, `|`,
//!    `&`, etc. — literals whose body is exactly one byte).
//! 2. It is a `Repeat` separator byte (e.g. `,` in CSV-style lists).
//!    These fall out of the recursive walk into the Repeat's inner.
//! 3. It is an `Alt` discriminator byte where the Alt's branches' FIRST
//!    sets are themselves single-byte literals (the branch leads with a
//!    single-byte `Literal`). The `AltDispatch.table` is populated from
//!    branch FIRST sets verbatim — byte-class regexes like
//!    `[a-zA-Z_]` contribute every letter byte. Admitting those would
//!    mine `[0..127]` on any grammar with an identifier rule. The
//!    correct admission reads the *branch node shape*, not the
//!    dispatch table, and only admits a byte when the branch's leading
//!    terminal is a single-byte `Literal`.
//! 4. The first byte of each `digraph` pair (`/` for `/*`, `*/`; `(`
//!    for `(*`, `*)`; `-` for `->`). Digraph mining is separate and
//!    walks exactly-two-byte literals; its first-byte admission is
//!    unconditional.
//!
//! EXCLUDED:
//!
//! - Bytes inside character classes, regex content, or string content.
//! - First bytes of any `IrNode::Regex` body (those are metacharacter
//!   classes, not delimiters).
//! - First bytes of any `IrNode::Literal` whose body is longer than
//!   one byte (`"true"`, `"false"`, `"null"`, identifiers, keywords —
//!   those go through the keyword-dispatch path, not the
//!   structural-bitmap kernel).
//!
//! When the set size is small (≤ 8, the nibble-LUT window), codegen in
//! `crates/core/src/generate/regex/emit/simd.rs` can emit a
//! grammar-wide bitmap kernel that every scan site routes through.
//!
//! ## Archaeology (AO.0.1 / AU.2.7 v2 / AW-III.W5.a / AW-IV.W1.γ)
//!
//! AW-III.W5.a extended the AU.2.7 v2 derivation with `digraph_mask`,
//! `digraph_pairs`, and `quote_classes`. AW-IV.W1.γ corrects the
//! single-byte mining definition: pre-γ the pass admitted the first
//! byte of every `Literal` regardless of length, plus every
//! `AltDispatch.table` entry whose slot was not `0xFF`. Both channels
//! over-flagged on grammars with multi-byte keywords (`t` from
//! `"true"`) and byte-class-first Alt branches (every letter from a
//! `Regex`-led branch's FIRST set). CSS L4 mined `[0..127]` as a
//! consequence; JSON mined `t`, `f`, `n` as phantom singletons. γ
//! restricts admission to the four categories above — CSS L4 drops to
//! the actual delimiter count (~15–25), JSON to 6–7, and the stage-1
//! SIMD scanner's structural index stops being degenerate.
//!
//! ## Digraph derivation (unchanged)
//!
//! Every exactly-2-byte literal in the grammar is a candidate digraph;
//! the pair survives if its first byte is also in the structural
//! single-byte set. Digraph mining is the ONE channel that admits a
//! multi-byte literal's first byte, and it does so via its own
//! unconditional re-insertion of `(first, _)` pairs into the byte set
//! after digraphs are mined — see below. Fully general across
//! grammar families; no per-grammar special cases.
//!
//! ## Quote-class derivation (unchanged)
//!
//! Every `IrNode::Regex` whose `RegexInfo::classification` is
//! `QuotedString { quote_char, .. }` contributes its `quote_char`.

use crate::{GrammarIR, IrNode, StringId};

use serde::{Deserialize, Serialize};

use std::collections::BTreeSet;

/// Width of the [`StructuralAlphabet`] bitmap fields, in `u64` words.
/// Four 64-bit words cover the full 256-byte universe (`u8` value
/// space) — the same shape `simd-scan` consumes.
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
    /// PMULL / shift-XOR quote-parity path in `simd-scan`.
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

    // Collect all literal-referenced StringIds and regex-referenced
    // StringIds. The `strings` table is also populated by rule names,
    // FnDescriptor pattern strings, and other interned data — we only
    // mine digraphs from the subset that actually feeds an
    // `IrNode::Literal`, and only mine quote classes from the subset
    // that feeds an `IrNode::Regex` (with a classified `RegexInfo`).
    let mut literal_sids: BTreeSet<StringId> = BTreeSet::new();
    let mut regex_sids: BTreeSet<StringId> = BTreeSet::new();
    for rule in &ir.rules {
        collect_node_sids(&rule.body, &mut literal_sids, &mut regex_sids);
    }

    // Digraph mining: harvest exactly-2-byte *literals* in the
    // grammar. Multi-byte literals (`true`, `false`, `null`,
    // identifiers) are not digraphs — those go through the keyword
    // dispatch table, not the structural-bitmap kernel. Regex
    // patterns are also excluded (their bytes describe
    // metacharacters, not delimiters). No hardcoded candidate list —
    // the data is the data. Every 2-byte literal is a candidate; no
    // pre-mining gate on `byte_set` — under the corrected W1.γ
    // single-byte mining definition, the digraph openers for `/*`,
    // `*/`, `(*`, `*)`, `->` are NOT themselves single-byte
    // `Literal`s in the grammar and would be filtered out. The
    // unconditional first-byte re-insertion below keeps every digraph
    // opener in the structural set regardless. Mining is
    // deduplicated and sorted for determinism.
    let mut digraph_set: BTreeSet<(u8, u8)> = BTreeSet::new();
    for sid in &literal_sids {
        let bytes = ir.strings[*sid as usize].as_bytes();
        if bytes.len() != 2 {
            continue;
        }
        digraph_set.insert((bytes[0], bytes[1]));
    }

    // Add digraph first-bytes to the structural set so the bitmap
    // kernel sees every digraph opener even if the byte was not
    // already a single-byte terminator (e.g. `-` in BBNF's `->` arrow
    // when no rule uses `-` as a leaf alone; `/` for CSS `/* */`).
    for (first, _) in &digraph_set {
        byte_set.insert(*first);
    }

    let digraphs: Vec<(u8, u8)> = digraph_set.iter().copied().collect();
    let digraph_mask = build_byte_bitmap(digraphs.iter().map(|(a, _)| *a));

    // Quote-class mining: for each regex-referenced StringId, look up
    // `regex_info` and admit the `quote_char` of any pattern classified
    // as `RegexClass::QuotedString`. The classifier is the single
    // source of truth; no string-pattern matching here.
    //
    // The classifier accepts only `"` and `'` as quote chars; other
    // delimiter-toggles (e.g. BBNF's `/regex/` literal) are not
    // surfaced today. The `simd-scan` parity kernel still works
    // for any byte set the IR exposes — the limit is purely the
    // mining boundary.
    let mut quote_classes: BTreeSet<u8> = BTreeSet::new();
    for sid in &regex_sids {
        if let Some(info) = ir.regex_info.get(sid) {
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

/// Recursively collect every interned `StringId` referenced from a
/// node tree, partitioned by node-kind so the alphabet pass can mine
/// digraphs (literal-only) and quote classes (regex-only) without
/// re-walking the IR per concern.
fn collect_node_sids(
    node: &IrNode,
    literals: &mut BTreeSet<StringId>,
    regexes: &mut BTreeSet<StringId>,
) {
    match node {
        IrNode::Literal(sid) => {
            literals.insert(*sid);
        }
        IrNode::Regex(sid) => {
            regexes.insert(*sid);
        }
        IrNode::Alt(branches, _) => {
            for b in branches {
                collect_node_sids(&b.node, literals, regexes);
            }
        }
        IrNode::Seq(children) => {
            for c in children {
                collect_node_sids(c, literals, regexes);
            }
        }
        IrNode::Repeat { inner, .. }
        | IrNode::OptionalWhitespace(inner)
        | IrNode::Negate(inner)
        | IrNode::Map { inner, .. } => collect_node_sids(inner, literals, regexes),
        IrNode::Skip(a, b) | IrNode::Next(a, b) | IrNode::Minus(a, b) => {
            collect_node_sids(a, literals, regexes);
            collect_node_sids(b, literals, regexes);
        }
        IrNode::TokenDispatch {
            token,
            arms,
            fallback,
        } => {
            collect_node_sids(token, literals, regexes);
            for arm in arms {
                collect_node_sids(&arm.continuation, literals, regexes);
            }
            collect_node_sids(fallback, literals, regexes);
        }
        IrNode::Epsilon | IrNode::Ref(_) => {}
    }
}

/// Walk a node tree and harvest bytes that delineate parse-tree
/// structure. Admission rules:
///
/// - `Literal(sid)` — admits `strings[sid][0]` iff the literal is
///   exactly one byte long. Multi-byte literals do NOT contribute
///   their first byte here; only the digraph mining (in
///   `compute_structural_alphabet`) admits a multi-byte first-byte,
///   and only when the literal is exactly two bytes.
/// - `Regex(_)` — contributes no bytes. The regex body's first-byte
///   classes describe metacharacters, not parse-tree delimiters.
/// - `Alt(branches, _)` — the dispatch table (if present) reflects
///   branch FIRST sets, which over-flag when a branch starts with a
///   byte-class regex. Instead of admitting every `dispatch.table[b]
///   != 0xFF`, we admit only branches whose leading terminal is itself
///   a single-byte `Literal` — that byte *is* a structural
///   discriminator; everything else is consumed by the branch's
///   non-structural prefix (regex, multi-byte literal, etc.) and
///   never reaches the scanner's inner loop as a terminator.
/// - `Repeat { inner, .. }` — recurses into `inner`. Separator bytes
///   (e.g. `,` in `list = item ("," item)*`) are reached through the
///   inner walk — no extra admission logic needed; the inner's own
///   single-byte literal terminals are admitted as leaf `Literal`s.
/// - All other combinators recurse structurally.
fn collect_bytes(node: &IrNode, ir: &GrammarIR, bytes: &mut BTreeSet<u8>) {
    match node {
        IrNode::Alt(branches, _dispatch_opt) => {
            // The `_dispatch_opt.table` is derived from branch FIRST
            // sets; admitting every populated slot over-flags for
            // branches that lead with a byte-class regex. Instead,
            // examine each branch's IR shape directly — a branch
            // whose leading terminal is a single-byte `Literal`
            // contributes that byte as a structural discriminator.
            // Everything else (Regex-led, multi-byte-literal-led,
            // Ref-led into a non-single-byte-terminal rule) recurses
            // into its body and contributes only what that body
            // itself exposes under the same rules.
            for b in branches {
                if let Some(byte) = leading_single_byte_literal(&b.node, ir) {
                    bytes.insert(byte);
                }
                collect_bytes(&b.node, ir, bytes);
            }
        }
        IrNode::Literal(sid) => {
            // Admit the first byte ONLY for single-byte literals.
            // Multi-byte literals are keyword-dispatch material, not
            // structural-scanner terminators. Digraph mining (in
            // `compute_structural_alphabet`) handles 2-byte literals
            // separately by re-inserting first-bytes after mining.
            let literal_bytes = ir.strings[*sid as usize].as_bytes();
            if literal_bytes.len() == 1 {
                bytes.insert(literal_bytes[0]);
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
            // Recurse into all of `token`, each arm's continuation,
            // and the fallback. The actual structural bytes land via
            // the single-byte-literal admission inside those subtrees.
            collect_bytes(token, ir, bytes);
            for arm in arms {
                collect_bytes(&arm.continuation, ir, bytes);
            }
            collect_bytes(fallback, ir, bytes);
        }
        IrNode::Regex(_) | IrNode::Epsilon | IrNode::Ref(_) => {}
    }
}

/// If `node`'s leading terminal is a single-byte `Literal`, return its
/// byte; otherwise `None`. "Leading terminal" unwraps transparent
/// wrappers (`Map`, `OptionalWhitespace`) and descends into the first
/// child of structural composites (`Seq` first child, `Skip`/`Next`
/// left operand). Does NOT cross rule boundaries (`Ref` returns
/// `None`) — a branch whose FIRST set comes from a referenced rule
/// may expose a byte class far wider than any single-byte terminator
/// we could admit here; that analysis belongs in per-rule mining, not
/// per-Alt-branch.
fn leading_single_byte_literal(node: &IrNode, ir: &GrammarIR) -> Option<u8> {
    match node {
        IrNode::Literal(sid) => {
            let bytes = ir.strings[*sid as usize].as_bytes();
            if bytes.len() == 1 {
                Some(bytes[0])
            } else {
                None
            }
        }
        IrNode::Seq(children) => children
            .iter()
            .find(|c| !matches!(c, IrNode::Epsilon))
            .and_then(|c| leading_single_byte_literal(c, ir)),
        IrNode::Skip(a, _) | IrNode::Next(a, _) => leading_single_byte_literal(a, ir),
        IrNode::Map { inner, .. } | IrNode::OptionalWhitespace(inner) => {
            leading_single_byte_literal(inner, ir)
        }
        // Alt, Repeat, Regex, Ref, Epsilon, Negate, Minus, TokenDispatch
        // — none of these expose a single leading single-byte literal
        // byte at this level.
        _ => None,
    }
}

// Tests live in tests/structural_alphabet_extended.rs (crate-level).
