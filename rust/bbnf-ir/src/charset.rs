//! 128-bit ASCII character set for FIRST set representation.
//!
//! Uses `[u64; 2]` for compact storage and efficient bitwise operations.
//! Serializable via serde for WASM boundary transfer.

use serde::{Deserialize, Serialize};

/// A compact 128-bit bitset representing a subset of ASCII characters (0..127).
#[derive(Serialize, Deserialize, Clone, Debug, Default, PartialEq, Eq)]
pub struct CharSet128 {
    /// Two 64-bit words covering bytes 0..63 and 64..127.
    pub bits: [u64; 2],
}

impl CharSet128 {
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a single byte to the set.
    pub fn add(&mut self, code: u8) {
        if code >= 128 {
            return;
        }
        let word = (code / 64) as usize;
        let bit = code % 64;
        self.bits[word] |= 1u64 << bit;
    }

    /// Check if a byte is in the set.
    pub fn has(&self, code: u8) -> bool {
        if code >= 128 {
            return false;
        }
        let word = (code / 64) as usize;
        let bit = code % 64;
        (self.bits[word] >> bit) & 1 != 0
    }

    /// Add a range of bytes.
    pub fn add_range(&mut self, from: u8, to: u8) {
        for code in from..=to {
            self.add(code);
        }
    }

    /// Union with another set.
    pub fn union(&mut self, other: &CharSet128) {
        self.bits[0] |= other.bits[0];
        self.bits[1] |= other.bits[1];
    }

    /// Check if two sets are disjoint.
    pub fn is_disjoint(&self, other: &CharSet128) -> bool {
        (self.bits[0] & other.bits[0]) == 0 && (self.bits[1] & other.bits[1]) == 0
    }

    /// Compute the intersection of two sets.
    pub fn intersection(&self, other: &CharSet128) -> CharSet128 {
        CharSet128 {
            bits: [self.bits[0] & other.bits[0], self.bits[1] & other.bits[1]],
        }
    }

    /// Check if the set is empty.
    pub fn is_empty(&self) -> bool {
        self.bits[0] == 0 && self.bits[1] == 0
    }

    /// Count the number of elements in the set.
    pub fn len(&self) -> usize {
        (self.bits[0].count_ones() + self.bits[1].count_ones()) as usize
    }

    /// Iterate over the byte values in the set.
    pub fn iter(&self) -> CharSet128Iter<'_> {
        CharSet128Iter {
            set: self,
            current: 0,
        }
    }

    /// Convert from the existing `[u32; 4]` CharSet representation.
    pub fn from_u32x4(bits: &[u32; 4]) -> Self {
        CharSet128 {
            bits: [
                (bits[0] as u64) | ((bits[1] as u64) << 32),
                (bits[2] as u64) | ((bits[3] as u64) << 32),
            ],
        }
    }

    /// Return the complement restricted to printable ASCII (32..127).
    pub fn complement_printable(&self) -> CharSet128 {
        let mut cs = CharSet128::new();
        for code in 32u8..127 {
            if !self.has(code) {
                cs.add(code);
            }
        }
        cs
    }

    /// Convert to the existing `[u32; 4]` CharSet representation.
    pub fn to_u32x4(&self) -> [u32; 4] {
        [
            self.bits[0] as u32,
            (self.bits[0] >> 32) as u32,
            self.bits[1] as u32,
            (self.bits[1] >> 32) as u32,
        ]
    }
}

/// Iterator over the byte values in a [`CharSet128`].
pub struct CharSet128Iter<'a> {
    set: &'a CharSet128,
    current: u8,
}

impl Iterator for CharSet128Iter<'_> {
    type Item = u8;

    fn next(&mut self) -> Option<u8> {
        while self.current < 128 {
            let c = self.current;
            self.current += 1;
            if self.set.has(c) {
                return Some(c);
            }
        }
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic_operations() {
        let mut set = CharSet128::new();
        assert!(set.is_empty());

        set.add(b'a');
        set.add(b'z');
        assert!(set.has(b'a'));
        assert!(set.has(b'z'));
        assert!(!set.has(b'b'));
        assert_eq!(set.len(), 2);
    }

    #[test]
    fn union_and_disjoint() {
        let mut a = CharSet128::new();
        a.add(b'a');
        a.add(b'b');

        let mut b = CharSet128::new();
        b.add(b'c');
        b.add(b'd');

        assert!(a.is_disjoint(&b));

        a.union(&b);
        assert!(a.has(b'a'));
        assert!(a.has(b'd'));
        assert_eq!(a.len(), 4);
    }

    #[test]
    fn roundtrip_u32x4() {
        let mut set = CharSet128::new();
        set.add(b'A');
        set.add(b'Z');
        set.add(b'0');
        set.add(b'9');

        let u32x4 = set.to_u32x4();
        let roundtripped = CharSet128::from_u32x4(&u32x4);
        assert_eq!(set, roundtripped);
    }

    #[test]
    fn iter() {
        let mut set = CharSet128::new();
        set.add(b'x');
        set.add(b'y');
        set.add(b'z');

        let collected: Vec<u8> = set.iter().collect();
        assert_eq!(collected, vec![b'x', b'y', b'z']);
    }
}
