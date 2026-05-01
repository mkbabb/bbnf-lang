//! BitsetDomain — O(1) domain for non-negative integers, isomorphic to Python's BitsetDomain.

use super::traits::Domain;

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
    pub fn new(values: impl IntoIterator<Item = u32>) -> Self {
        let mut bits: u128 = 0;
        for v in values {
            debug_assert!(v < 128, "BitsetDomain supports values 0..128");
            bits |= 1u128 << v;
        }
        Self { bits }
    }

    /// Create an empty bitset domain.
    pub fn empty() -> Self {
        Self { bits: 0 }
    }

    /// Create a domain containing `0..n`.
    pub fn range(n: u32) -> Self {
        debug_assert!(n <= 128);
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
        debug_assert!(*val < 128, "BitsetDomain supports values 0..128");
        self.bits |= 1u128 << val;
    }

    fn values(&self) -> Vec<u32> {
        self.iter().collect()
    }

    fn iter(&self) -> impl Iterator<Item = u32> {
        BitsetIter { bits: self.bits }
    }

    fn singleton_value(&self) -> Option<u32> {
        if self.bits.count_ones() == 1 {
            Some(self.bits.trailing_zeros())
        } else {
            None
        }
    }
}

/// Iterator over set bits in a `BitsetDomain`.
pub struct BitsetIter {
    bits: u128,
}

impl Iterator for BitsetIter {
    type Item = u32;

    fn next(&mut self) -> Option<u32> {
        if self.bits == 0 {
            None
        } else {
            let lowest = self.bits.trailing_zeros();
            self.bits &= self.bits - 1; // clear lowest set bit
            Some(lowest)
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let count = self.bits.count_ones() as usize;
        (count, Some(count))
    }
}

impl ExactSizeIterator for BitsetIter {}
