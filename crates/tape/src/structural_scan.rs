//! `structural_scan` — pre-pass byte-class index for the parser entry.
//!
//! AY.W1.3 — activates the long-orphaned [`GrammarProfile::structural_alphabet`]
//! field. The mining pass [`compute_structural_alphabet`] has shipped
//! since W0b without a runtime consumer; W1.3 closes the
//! substrate-with-consumer cycle (invariant 22) by emitting a
//! `scan_structural` call at every `<Parser>::parse` entry that
//! materialises a [`StructuralIndex`] over the input bytes.
//!
//! # Mechanism
//!
//! The structural alphabet is the per-grammar set of bytes that
//! delineate parse-tree structure (`{`, `}`, `:`, `,`, `[`, `]` for
//! JSON; `;`, `:`, `{`, `}`, `,`, `(`, `)` for CSS L4; etc. — see
//! [`bbnf_ir::passes::sets::structural_alphabet`] for the mining
//! contract). [`scan_structural`] iterates the input once, classifying
//! each byte against a 256-bit bitmap derived from the alphabet, and
//! produces a [`StructuralIndex`] holding `(position, kind)` pairs for
//! every match.
//!
//! The result is stored on the per-grammar `ScanState` so per-rule
//! parse fns can query it via offset-bounded lookups
//! ([`next_structural_at_or_after`] does the binary search).
//!
//! # Architectural fit
//!
//! Pre-W1.3, the [`StructuralIndex`] type lived in [`crate::stage1`]
//! as the wire contract between the unrelated DTA-driver-era SIMD
//! scanner and the cursor consumer; that consumer has retired with
//! the DTA prune (W0). W1.3 re-purposes the type as the substrate for
//! the per-grammar scan: same column shape (`positions: Vec<u32>` +
//! `kinds: Vec<u8>`), new producer ([`scan_structural`]), new
//! consumer (per-grammar `ScanState::structural_index`).
//!
//! [`GrammarProfile::structural_alphabet`]: crate::profile::GrammarProfile::structural_alphabet
//! [`compute_structural_alphabet`]: bbnf_ir::passes::sets::structural_alphabet::compute_structural_alphabet

use crate::stage1::StructuralIndex;

/// Heuristic over-approximation of the structural-byte density. Most
/// grammars (JSON, CSS L4, BBNF) hit between 5% (sparse JSON arrays)
/// and 25% (dense CSS declarations); the 1/8 reservation amortises
/// the worst case without over-allocating on cold-cache parses.
const EXPECTED_STRUCTURAL_DENSITY_INV: usize = 8;

/// Build a 256-bit bitmap from a sorted alphabet so the per-byte test
/// is one indexed load + one mask. Word `i` covers bytes
/// `64*i .. 64*(i+1)`.
#[inline]
fn build_alphabet_bitmap(alphabet: &[u8]) -> [u64; 4] {
    let mut bitmap = [0u64; 4];
    for &b in alphabet {
        let word = (b >> 6) as usize;
        let bit = b & 0x3F;
        bitmap[word] |= 1u64 << bit;
    }
    bitmap
}

/// Build a side-table mapping each alphabet byte to its sorted
/// position in `alphabet` (the `kinds[i]` value emitted alongside
/// each match). Bytes outside the alphabet remain `0xFF`.
#[inline]
fn build_alphabet_index(alphabet: &[u8]) -> [u8; 256] {
    let mut index = [0xFFu8; 256];
    for (rank, &b) in alphabet.iter().enumerate() {
        // Cardinality is bounded by the alphabet's 256-byte universe;
        // `rank` always fits in `u8`.
        index[b as usize] = rank as u8;
    }
    index
}

/// Scan `input` for every byte in `alphabet`, producing a
/// [`StructuralIndex`] of `(position, alphabet_rank)` pairs.
///
/// `kinds[i]` holds the alphabet's sorted rank of the matched byte
/// (0..alphabet.len()), not the byte value itself — downstream
/// consumers querying "what kind of structural byte is at this
/// position" get a dense small-integer discriminant suitable for jump
/// tables. The original byte value is recovered via `alphabet[rank]`.
///
/// Empty alphabets short-circuit to an empty index without iterating
/// the input.
#[inline]
pub fn scan_structural(input: &[u8], alphabet: &[u8]) -> StructuralIndex {
    if alphabet.is_empty() {
        return StructuralIndex::new();
    }

    let bitmap = build_alphabet_bitmap(alphabet);
    let alphabet_rank = build_alphabet_index(alphabet);

    let mut index = StructuralIndex::with_capacity(
        input.len() / EXPECTED_STRUCTURAL_DENSITY_INV,
    );

    for (pos, &b) in input.iter().enumerate() {
        let word = (b >> 6) as usize;
        let bit = b & 0x3F;
        if (bitmap[word] >> bit) & 1 == 1 {
            // Safety: `alphabet_rank[b]` is `0..alphabet.len()` for
            // every byte in the alphabet (the only bytes we reach
            // here); the bitmap-AND-true predicate ensures the rank
            // is not the `0xFF` sentinel.
            index.push(pos as u32, alphabet_rank[b as usize]);
        }
    }
    index
}

/// Return the smallest position in `index.positions` that is `≥ from`,
/// or `None` if every position is below `from`. Binary search; the
/// position column is monotonically increasing by construction.
#[inline]
pub fn next_structural_at_or_after(
    index: &StructuralIndex,
    from: u32,
) -> Option<u32> {
    match index.positions.binary_search(&from) {
        Ok(idx) => Some(index.positions[idx]),
        Err(idx) => index.positions.get(idx).copied(),
    }
}
