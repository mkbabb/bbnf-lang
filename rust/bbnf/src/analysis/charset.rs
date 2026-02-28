//! CharSet — 128-bit ASCII bitset.

/// A compact 128-bit bitset representing a subset of ASCII characters (0..127).
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct CharSet {
    pub(crate) bits: [u32; 4],
}

impl CharSet {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add(&mut self, code: u8) {
        if code >= 128 {
            return;
        }
        let word = (code / 32) as usize;
        let bit = code % 32;
        self.bits[word] |= 1 << bit;
    }

    pub fn has(&self, code: u8) -> bool {
        if code >= 128 {
            return false;
        }
        let word = (code / 32) as usize;
        let bit = code % 32;
        (self.bits[word] >> bit) & 1 != 0
    }

    pub fn add_range(&mut self, from: u8, to: u8) {
        for code in from..=to {
            self.add(code);
        }
    }

    pub fn union(&mut self, other: &CharSet) {
        for i in 0..4 {
            self.bits[i] |= other.bits[i];
        }
    }

    pub fn is_disjoint(&self, other: &CharSet) -> bool {
        for i in 0..4 {
            if self.bits[i] & other.bits[i] != 0 {
                return false;
            }
        }
        true
    }

    pub fn intersection(&self, other: &CharSet) -> CharSet {
        let mut result = CharSet::new();
        for i in 0..4 {
            result.bits[i] = self.bits[i] & other.bits[i];
        }
        result
    }

    pub fn is_empty(&self) -> bool {
        self.bits.iter().all(|&w| w == 0)
    }

    pub fn iter(&self) -> CharSetIter<'_> {
        CharSetIter {
            set: self,
            current: 0,
        }
    }

    pub fn len(&self) -> usize {
        self.bits.iter().map(|w| w.count_ones() as usize).sum()
    }
}

/// Iterator over the character codes in a [`CharSet`].
pub struct CharSetIter<'a> {
    set: &'a CharSet,
    current: u8,
}

impl Iterator for CharSetIter<'_> {
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
