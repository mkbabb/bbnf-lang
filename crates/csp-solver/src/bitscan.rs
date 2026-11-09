//! Shared find-lowest-set-bit-and-clear primitive.
//!
//! `BitsetIter` (`domain/bitset.rs`, a single `u128` value-domain snapshot)
//! and `BitsetWorklist` (`solver/ac3.rs`, a `Vec<u64>` constraint arc-queue)
//! both implement the identical idiom — `trailing_zeros` + `bits &= bits -
//! 1` — over genuinely different word widths and container shapes. Per the
//! Pass-1 audit (`rust-domain.md` P2, `rust-propagation.md` §5.4 / P3-1),
//! unifying the *container types* would be the wrong fix (it would force
//! the domain to pay `Vec` indirection it doesn't need, or force the
//! worklist into a fixed-width cap it can't guarantee) — the right-sized
//! fix is one shared scan primitive used by both.

/// A machine word that supports the find-lowest-set-bit-and-clear scan.
pub(crate) trait BitWord: Copy + Eq {
    const ZERO: Self;
    fn trailing_zeros_usize(self) -> usize;
    fn clear_lowest(self) -> Self;
}

macro_rules! impl_bit_word {
    ($($t:ty),+) => {
        $(
            impl BitWord for $t {
                const ZERO: Self = 0;
                #[inline]
                fn trailing_zeros_usize(self) -> usize {
                    self.trailing_zeros() as usize
                }
                #[inline]
                fn clear_lowest(self) -> Self {
                    self & self.wrapping_sub(1)
                }
            }
        )+
    };
}

impl_bit_word!(u64, u128);

/// Pop the lowest set bit out of `word`, returning its index, or `None` if
/// `word` is already zero. Shared by `BitsetIter::next` (single `u128`
/// scan) and `BitsetWorklist::pop`'s per-word scan (`Vec<u64>`).
#[inline]
pub(crate) fn pop_lowest_bit<W: BitWord>(word: &mut W) -> Option<usize> {
    if *word == W::ZERO {
        None
    } else {
        let idx = word.trailing_zeros_usize();
        *word = word.clear_lowest();
        Some(idx)
    }
}
