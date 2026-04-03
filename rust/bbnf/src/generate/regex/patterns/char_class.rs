//! Character class analysis — computes included/excluded byte sets and
//! determines the optimal emission strategy given a `CostModel`.

use crate::generate::regex::cost_model::{CostModel, LengthHint};

/// Emission strategy for a character class pattern.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CharClassStrategy {
    /// Use memchr1/2/3 on excluded bytes (inverted scan).
    Memchr { num_needles: usize },
    /// Use SIMD nibble-LUT on excluded bytes.
    NibbleLut,
    /// Use scalar byte-predicate loop.
    PredicateLoop,
}

/// Analysis result for a positive or negated character class.
#[derive(Debug, Clone)]
pub struct CharClassAnalysis {
    /// Bytes included in the class (for positive classes).
    pub included: Vec<u8>,
    /// Bytes excluded from the class (complement of included within 0..128 ASCII).
    pub excluded: Vec<u8>,
    /// Recommended emission strategy.
    pub strategy: CharClassStrategy,
}

impl CharClassAnalysis {
    /// Analyze a positive character class given its included byte ranges.
    ///
    /// Computes the excluded byte set (ASCII complement) and selects
    /// the optimal emission strategy based on the cost model and length hint.
    pub fn from_included_ranges(
        ranges: &[(u8, u8)],
        cost: &CostModel,
        length_hint: LengthHint,
    ) -> Self {
        let mut included = Vec::new();
        for &(lo, hi) in ranges {
            for b in lo..=hi {
                included.push(b);
            }
        }
        included.sort_unstable();
        included.dedup();

        // Compute excluded bytes (ASCII range 0..128)
        let mut included_set = [false; 256];
        for &b in &included {
            included_set[b as usize] = true;
        }
        let excluded: Vec<u8> = (0u8..128).filter(|b| !included_set[*b as usize]).collect();

        let strategy = select_strategy(excluded.len(), cost, length_hint);

        Self {
            included,
            excluded,
            strategy,
        }
    }

    /// Analyze a negated character class given its excluded bytes.
    pub fn from_excluded_bytes(excluded: &[u8], cost: &CostModel, length_hint: LengthHint) -> Self {
        let mut excluded = excluded.to_vec();
        excluded.sort_unstable();
        excluded.dedup();

        let mut excluded_set = [false; 256];
        for &b in &excluded {
            excluded_set[b as usize] = true;
        }
        let included: Vec<u8> = (0u8..128).filter(|b| !excluded_set[*b as usize]).collect();

        let strategy = select_strategy(excluded.len(), cost, length_hint);

        Self {
            included,
            excluded,
            strategy,
        }
    }
}

/// Select emission strategy based on excluded byte count, cost model, and length hint.
fn select_strategy(
    num_excluded: usize,
    cost: &CostModel,
    length_hint: LengthHint,
) -> CharClassStrategy {
    // Short matches prefer scalar to avoid memchr setup overhead (~15 cycles).
    if length_hint == LengthHint::Short {
        return CharClassStrategy::PredicateLoop;
    }

    if num_excluded <= cost.memchr_max_needles && num_excluded >= 1 {
        CharClassStrategy::Memchr {
            num_needles: num_excluded,
        }
    } else if num_excluded <= cost.nibble_lut_max_excluded {
        CharClassStrategy::NibbleLut
    } else {
        CharClassStrategy::PredicateLoop
    }
}
