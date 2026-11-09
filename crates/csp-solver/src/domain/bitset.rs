//! BitsetDomain — O(1) domain for non-negative integers, isomorphic to Python's BitsetDomain.

use super::traits::Domain;
use crate::bitscan::pop_lowest_bit;

/// A domain backed by a 128-bit bitmask.
///
/// Values must be non-negative integers in the range `0..128`.
/// All operations are O(1) via bitwise arithmetic and popcount.
/// Isomorphic to the Python `BitsetDomain`.
#[derive(Clone, PartialEq)]
pub struct BitsetDomain {
    bits: u128,
}

impl std::fmt::Debug for BitsetDomain {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let vals: Vec<u32> = self.values();
        write!(f, "BitsetDomain({vals:?})")
    }
}

impl BitsetDomain {
    /// Create a new bitset domain from an iterator of values.
    ///
    /// # Panics
    /// Panics if any value is `>= 128`. This is a **release-mode** `assert!`,
    /// not a `debug_assert!` (R7): a bare `1u128 << v` with `v >= 128` does not
    /// fault in release — Rust masks the shift to `v % 128`, silently aliasing
    /// out-of-range values onto valid bits. Since `new`/`range` are reachable
    /// from the published py/wasm bindings with caller-supplied values, the
    /// `0..128` invariant must fail loudly in every build. Both are
    /// construction-time (not hot), so the branch is free; see `add` for the
    /// hot-path rationale.
    pub fn new(values: impl IntoIterator<Item = u32>) -> Self {
        let mut bits: u128 = 0;
        for v in values {
            assert!(v < 128, "BitsetDomain supports values 0..128, got {v}");
            bits |= 1u128 << v;
        }
        Self { bits }
    }

    /// Create an empty bitset domain.
    pub fn empty() -> Self {
        Self { bits: 0 }
    }

    /// Create a domain containing `0..n`.
    ///
    /// # Panics
    /// Panics if `n > 128` (release-mode; see [`BitsetDomain::new`] for the R7
    /// rationale — `(1u128 << n) - 1` masks `n` to `n % 128` in release,
    /// silently truncating the range).
    pub fn range(n: u32) -> Self {
        assert!(n <= 128, "BitsetDomain::range supports 0..=128, got {n}");
        let bits = if n == 128 {
            u128::MAX
        } else {
            (1u128 << n) - 1
        };
        Self { bits }
    }

    /// Raw bits access.
    pub fn bits(&self) -> u128 {
        self.bits
    }

    /// Union with another bitset domain.
    pub fn union_with(&mut self, other: &Self) {
        self.bits |= other.bits;
    }

    /// Intersection with another bitset domain.
    pub fn intersect_with(&mut self, other: &Self) {
        self.bits &= other.bits;
    }

    /// Difference: remove all values present in `other`.
    pub fn difference_with(&mut self, other: &Self) {
        self.bits &= !other.bits;
    }

    /// Iterate over set bits, yielding each value.
    pub fn iter(&self) -> BitsetIter {
        BitsetIter { bits: self.bits }
    }
}

impl Domain for BitsetDomain {
    type Value = u32;

    fn size(&self) -> usize {
        self.bits.count_ones() as usize
    }

    fn contains(&self, val: &u32) -> bool {
        if *val >= 128 {
            return false;
        }
        (self.bits & (1u128 << val)) != 0
    }

    fn remove(&mut self, val: &u32) -> bool {
        if *val >= 128 {
            return false;
        }
        let mask = 1u128 << val;
        let was_present = (self.bits & mask) != 0;
        self.bits &= !mask;
        was_present
    }

    fn add(&mut self, val: &u32) {
        // Release-mode check (R7): even on the hot restore path, `add` must not
        // silently alias `val >= 128` onto `val % 128`. The branch is perfectly
        // predicted — every internal caller restores a value that was itself in
        // `0..128` — and is dwarfed by the bit write, so no unchecked fast path
        // is warranted; profiling does not justify splitting the API.
        assert!(*val < 128, "BitsetDomain supports values 0..128, got {val}");
        self.bits |= 1u128 << val;
    }

    fn values(&self) -> Vec<u32> {
        self.iter().collect()
    }

    fn iter(&self) -> impl Iterator<Item = u32> + use<> {
        BitsetIter { bits: self.bits }
    }

    fn singleton_value(&self) -> Option<u32> {
        if self.bits.count_ones() == 1 {
            Some(self.bits.trailing_zeros())
        } else {
            None
        }
    }

    /// O(1) restrict: clear every bit but `val`'s in one bitwise AND,
    /// instead of the trait default's O(domain size) iterate-and-clear
    /// loop. Returns a `BitsetIter` (zero-alloc) over the cleared bits.
    fn restrict_to(&mut self, val: &u32) -> impl Iterator<Item = u32> + use<> {
        // Release-mode check (R7): same shift-aliasing hazard as `add` — a
        // masked `1u128 << (*val % 128)` would keep the wrong bit. Well-
        // predicted hot-path branch (callers restrict to an in-domain value).
        assert!(*val < 128, "BitsetDomain supports values 0..128, got {val}");
        let keep_mask = 1u128 << *val;
        let pruned = self.bits & !keep_mask;
        self.bits &= keep_mask;
        BitsetIter { bits: pruned }
    }
}

/// Iterator over set bits in a `BitsetDomain`.
pub struct BitsetIter {
    bits: u128,
}

impl Iterator for BitsetIter {
    type Item = u32;

    fn next(&mut self) -> Option<u32> {
        pop_lowest_bit(&mut self.bits).map(|idx| idx as u32)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let count = self.bits.count_ones() as usize;
        (count, Some(count))
    }
}

impl ExactSizeIterator for BitsetIter {}
