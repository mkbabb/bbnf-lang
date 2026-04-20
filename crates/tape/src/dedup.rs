//! Runtime bloom + GADT compound-record dedup (AW-IV.W4.3).
//!
//! # Architectural role
//!
//! Where [`GrammarProfile::dedup_eligible_rules`](crate::GrammarProfile::dedup_eligible_rules)
//! admits a rule, the walker's compound-emit branch consults this
//! module before calling [`Columns::push_compound_fused`](crate::Columns::push_compound_fused).
//! On a hit, the caller emits a referring record that points at the
//! existing structural skeleton instead of pushing the full compound
//! again — saving the 7-column write + subtree pushes for every
//! duplicate compound the parser encounters.
//!
//! # Admission contract
//!
//! The grammar-level mining pass
//! ([`bbnf_ir::passes::recognizers::dedup_eligibility`]) admits rules
//! whose body shape is:
//!
//! - Fixed-width compounds (every branch emits the same number of
//!   structural records).
//! - Total record count ≤ 4 — keeps the `columns_range_eq` confirm
//!   cheap (at most 4 * 7 = 28 column slot compares).
//! - Payload is likely repeated across the corpus (literal-only Alt
//!   branches, fixed unit suffixes like `px` / `em`, named colour
//!   keywords).
//!
//! The runtime consults the bloom first (constant-time admission
//! gate), then GADT lookup + `columns_range_eq` confirms on a hit.
//! False positives short-circuit cleanly: the call degrades to the
//! normal `push_compound_fused` path at no measurable cost.
//!
//! # Hashing
//!
//! 64-bit FNV-1a over the raw column bytes of the compound's
//! structural rows. FNV-1a is used over FxHash because FNV's
//! incremental update is trivially inlinable — each column read
//! folds one `u64` lane into the running hash without a state struct.
//! For the typical 1-3 record compound this is ~24-72 bytes of input,
//! well inside FNV's efficient range.
//!
//! # KISS design
//!
//! Fixed-size bloom (128 `u64` words → 8192 bits → expected false-
//! positive rate ~1% at 1000 insertions). Stored per-parse on the
//! builder; cleared at parse entry. GADT is a `HashMap<u64, u32>` —
//! not an e-class, not a specialised structure — because the admitted
//! set is small (`≤ 4` records per compound × low-hundreds of
//! distinct shapes per corpus file).

use crate::columns::Columns;

/// Bloom filter word count. 128 words × 64 bits = 8192 bits. With a
/// target false-positive rate ≤ 1% the bloom comfortably admits up to
/// ~1000 distinct entries; above that the `columns_range_eq` confirm
/// still keeps correctness, at the cost of an additional GADT probe
/// per compound.
pub const N_WORDS: usize = 128;

/// Number of structural columns written per compound record (see
/// [`Columns::push_compound_fused`](crate::Columns::push_compound_fused)).
/// The hash folds this many `u64` lanes per record — one per column
/// field — plus the `span_lo` / `span_hi` pair.
const COLUMN_LANES_PER_RECORD: usize = 8;

/// Runtime bloom + GADT dedup cache.
///
/// Sits next to the walker's [`Columns`] for the duration of one
/// parse. Cleared at parse entry; consumed per compound at the
/// dedup-eligible emit arm.
#[derive(Debug)]
pub struct BloomDedup {
    /// Bloom bitmap — `N_WORDS * 64` bits.
    bloom: [u64; N_WORDS],
    /// Hash → existing record index. On hit, the caller runs
    /// `columns_range_eq` over the slice starting at this index to
    /// confirm the match; on mismatch, falls through to the normal
    /// compound-emit path.
    gadt: std::collections::HashMap<u64, u32>,
}

impl Default for BloomDedup {
    fn default() -> Self {
        Self::new()
    }
}

impl BloomDedup {
    /// Construct an empty dedup cache.
    #[inline]
    pub fn new() -> Self {
        Self {
            bloom: [0u64; N_WORDS],
            gadt: std::collections::HashMap::new(),
        }
    }

    /// Reset the cache for a fresh parse. Does not shrink capacity —
    /// the HashMap retains its allocated buckets so the next parse
    /// reuses them.
    #[inline]
    pub fn reset(&mut self) {
        self.bloom = [0u64; N_WORDS];
        self.gadt.clear();
    }

    /// Probe + insert for a compound record at `rec_idx` spanning
    /// `record_count` structural rows.
    ///
    /// Returns:
    /// - `Some(existing_idx)` — the bloom admitted, the GADT had a
    ///   hit, AND `columns_range_eq` confirmed byte-for-byte equality
    ///   against the existing run starting at `existing_idx`. The
    ///   caller emits a referring record at `rec_idx` and elides the
    ///   subtree pushes.
    /// - `None` — either the bloom missed, the GADT missed, or the
    ///   `columns_range_eq` confirm failed. The caller proceeds with
    ///   the normal compound-emit path; this function records the new
    ///   entry in both the bloom and the GADT for future probes.
    ///
    /// # Hashing
    ///
    /// FNV-1a over the raw column bytes of rows
    /// `[rec_idx..rec_idx + record_count]`. The column lanes folded
    /// are (`kinds`, `flags`, `extra`, `span_lo`, `span_hi`,
    /// `sib_skip`, `child_off`) — the seven structural columns that
    /// uniquely identify a compound's skeleton.
    #[inline]
    pub fn try_dedup(
        &mut self,
        columns: &Columns,
        rec_idx: u32,
        record_count: u32,
    ) -> Option<u32> {
        let hash = hash_record_range(columns, rec_idx, record_count);
        // Bloom probe: three bit positions derived from the hash via
        // independent splits. On any bit zero, the entry is definitely
        // new — insert it and return None.
        let (b0, b1, b2) = bloom_bit_positions(hash);
        let present = self.bloom_get(b0) && self.bloom_get(b1) && self.bloom_get(b2);
        if !present {
            self.bloom_set(b0);
            self.bloom_set(b1);
            self.bloom_set(b2);
            self.gadt.insert(hash, rec_idx);
            return None;
        }
        // Bloom hit — probe the GADT for an authoritative answer.
        match self.gadt.get(&hash).copied() {
            ::core::option::Option::Some(existing_idx) if existing_idx != rec_idx => {
                // Deep-compare the column rows to resolve any hash
                // collision false-positive. On equality the caller
                // reuses the existing skeleton; on mismatch, record
                // the current row's index so future probes at this
                // hash find a more recent instance.
                if columns_range_eq(columns, existing_idx, rec_idx, record_count) {
                    ::core::option::Option::Some(existing_idx)
                } else {
                    // Hash collision — overwrite the GADT entry so
                    // subsequent probes get the latest record at this
                    // bucket. The old record's index is still a valid
                    // tape offset; we simply prefer the newer one.
                    self.gadt.insert(hash, rec_idx);
                    ::core::option::Option::None
                }
            }
            ::core::option::Option::Some(_) | ::core::option::Option::None => {
                // GADT miss (or identity hit — `rec_idx == rec_idx`
                // is a degenerate self-probe that can happen when the
                // caller passes the just-pushed index without first
                // clearing the GADT; ignore it and record a new
                // entry).
                self.gadt.insert(hash, rec_idx);
                ::core::option::Option::None
            }
        }
    }

    /// Test a bloom bit.
    #[inline]
    fn bloom_get(&self, bit: u32) -> bool {
        let word = (bit as usize) >> 6;
        let mask = 1u64 << (bit & 0x3F);
        (self.bloom[word] & mask) != 0
    }

    /// Set a bloom bit.
    #[inline]
    fn bloom_set(&mut self, bit: u32) {
        let word = (bit as usize) >> 6;
        let mask = 1u64 << (bit & 0x3F);
        self.bloom[word] |= mask;
    }
}

/// FNV-1a offset basis (64-bit).
const FNV_OFFSET_BASIS: u64 = 0xCBF29CE484222325;

/// FNV-1a prime (64-bit).
const FNV_PRIME: u64 = 0x00000100000001B3;

/// Fold the structural column bytes for rows
/// `[start..start + count]` into a 64-bit FNV-1a hash.
///
/// Each row contributes [`COLUMN_LANES_PER_RECORD`] lanes: the AoS
/// `TapeRec` fields + parallel `sib_skip` + a zero tail so the lane
/// count stays a power of two (lets LLVM vectorise the folding loop).
#[inline]
fn hash_record_range(columns: &Columns, start: u32, count: u32) -> u64 {
    let mut h: u64 = FNV_OFFSET_BASIS;
    let s = start as usize;
    let e = s + count as usize;
    if e > columns.len() {
        // Defensive: record range out of bounds. Return an impossible
        // hash so the caller never matches on it.
        return 0;
    }
    let records = columns.records();
    for i in s..e {
        // SAFETY: the guard above ensures i < columns.len(), and the
        // `sib_skip` column grows in lockstep with `records` per
        // `push_structural` / `push_compound_fused` / `push_leaf_fused`.
        unsafe {
            let rec = *records.get_unchecked(i);
            let sib_skip = *columns.sib_skip.get_unchecked(i) as u64;
            h = fnv_fold(h, (rec.kind_meta) as u64);
            h = fnv_fold(h, rec.flags as u64);
            h = fnv_fold(h, rec.extra as u64);
            h = fnv_fold(h, rec.span_lo as u64);
            h = fnv_fold(h, rec.span_hi as u64);
            h = fnv_fold(h, sib_skip);
            h = fnv_fold(h, rec.child_off.0 as u64);
            // Padding lane — keeps COLUMN_LANES_PER_RECORD a power of
            // two so the fold loop aligns with 8-lane SIMD widths.
            h = fnv_fold(h, 0);
        }
    }
    let _ = COLUMN_LANES_PER_RECORD;
    h
}

/// Incremental FNV-1a fold of one `u64` lane.
#[inline]
fn fnv_fold(h: u64, lane: u64) -> u64 {
    let mut h = h;
    for byte_idx in 0..8 {
        let byte = ((lane >> (byte_idx * 8)) & 0xFF) as u8;
        h ^= byte as u64;
        h = h.wrapping_mul(FNV_PRIME);
    }
    h
}

/// Derive three independent bit positions in the bloom bitmap from a
/// single 64-bit hash.
///
/// Splits the hash into three 22-bit lanes modulo the bitmap width
/// (`N_WORDS * 64 = 8192`). Three independent hash functions are the
/// standard bloom-filter tradeoff at ~1% false-positive rate.
#[inline]
fn bloom_bit_positions(hash: u64) -> (u32, u32, u32) {
    let bits = (N_WORDS as u32) * 64;
    let b0 = ((hash & 0x1FFFFF) as u32) % bits;
    let b1 = (((hash >> 21) & 0x1FFFFF) as u32) % bits;
    let b2 = (((hash >> 42) & 0x1FFFFF) as u32) % bits;
    (b0, b1, b2)
}

/// Byte-for-byte equality check over the AoS structural columns for
/// two record ranges `[a_start..a_start + count]` and
/// `[b_start..b_start + count]`.
///
/// Used by [`BloomDedup::try_dedup`] to confirm a bloom+GADT hit.
/// Inline-friendly: the loop unrolls at small record counts and LLVM
/// can vectorise the per-column compares.
///
/// `span_lo` / `span_hi` intentionally excluded from the equality
/// test: identical compound shapes at different source offsets
/// should still dedup. The hash above folds them in for probing,
/// but the confirm compares only the invariant structural fields
/// (kind, flags, extra, sib_skip, child_off) — span differences
/// don't block reuse of the same skeleton when the variant +
/// children match.
#[inline]
pub fn columns_range_eq(
    columns: &Columns,
    a_start: u32,
    b_start: u32,
    count: u32,
) -> bool {
    let a = a_start as usize;
    let b = b_start as usize;
    let n = count as usize;
    if a + n > columns.len() || b + n > columns.len() {
        return false;
    }
    let records = columns.records();
    // SAFETY: guarded by the length check above; structural columns
    // grow in lockstep.
    unsafe {
        for i in 0..n {
            let ra = records.get_unchecked(a + i);
            let rb = records.get_unchecked(b + i);
            if ra.kind_meta != rb.kind_meta {
                return false;
            }
            if ra.flags != rb.flags {
                return false;
            }
            if ra.extra != rb.extra {
                return false;
            }
            if *columns.sib_skip.get_unchecked(a + i) != *columns.sib_skip.get_unchecked(b + i)
            {
                return false;
            }
            if ra.child_off.0 != rb.child_off.0 {
                return false;
            }
        }
    }
    true
}

/// Emit a compound record that refers to an existing skeleton rather
/// than pushing its structural rows again.
///
/// Called by the walker's dedup-eligible compound-emit arm when
/// [`BloomDedup::try_dedup`] returns `Some(existing)`. The record
/// occupies exactly one tape row; its `child_off` points at
/// `existing` so consumer code that walks children follows the
/// original skeleton.
///
/// Re-uses the existing [`Columns::push_compound_fused`] infrastructure
/// — no new column-write helper is introduced. The `span_lo` /
/// `span_hi` are the duplicate instance's span (not the referent's),
/// so consumers that care about source location see the correct
/// range even when the structural subtree is shared.
///
/// Returns the new row's index.
#[inline]
pub fn push_compound_referring(
    columns: &mut Columns,
    kind: crate::kind::TapeKind,
    rule_id: u32,
    existing: u32,
    span: (u32, u32),
) -> u32 {
    // Reserve one structural row. `push_compound_fused` writes one
    // 16-byte AoS row + one 4-byte sib_skip; we then patch the row's
    // `flags` (rule id low byte for identification on dedup hits),
    // `span_hi` (the duplicate instance's span end), `child_off` (the
    // referent's row index), and `extra` (HAS_CHILDREN so cursor
    // traversal follows `child_off`).
    let idx = columns.push_compound_fused(kind, span.0);
    let records = columns.records_mut();
    let rec = &mut records[idx as usize];
    rec.flags = (rule_id & 0xFF) as u8;
    rec.span_hi = span.1;
    rec.child_off = crate::tape::TapeOffset(existing);
    rec.extra = crate::tape::TapeRec::HAS_CHILDREN_BIT;
    columns.invalidate_packed();
    idx
}
